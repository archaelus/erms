%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Message Archiving process
%% @end
%%%-------------------------------------------------------------------
-module(erms_msg_archiver).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").
-include_lib("mnesia_model.hrl").

%% API
-export([start_link/0, archive/1, param/1,
         truncate/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {log}).
-define(SERVER, ?MODULE).
-define(LOG, erms_msg_archiver_log).
-define(LOGFILENAME, "archived_msgs").
-define(LOGFILESIZE, 100*1024*1024). % 100Meg
-define(LOGFILECOUNT, 20).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

archive(MsgId) ->
    gen_server:cast(?SERVER, {archive, MsgId}).

truncate(i_really_mean_it) ->
    case whereis(?SERVER) of
        Pid when is_pid(Pid) ->
            {error, system_running};
        undefined ->
            {ok, Log} = open_disk_log([{name, ?LOG},
                                       {file, param(file)},
                                       {repair, truncate},
                                       {type, wrap},
                                       {size, param(size)},
                                       {format, internal},
                                       {notify, true},
                                       {mode, read_write}
                                      ]),
            disk_log:truncate(Log),
            disk_log:close(Log)
    end.
            
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Log} = open_disk_log([{name, ?LOG},
                               {file, param(file)},
                               {repair, true},
                               {type, wrap},
                               {size, param(size)},
                               {format, internal},
                               {notify, true},
                               {mode, read_write}
                              ]),
    {ok, #state{log=Log}}.

            
%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({archive, Id}, State) ->
%    ?INFO("Archiving message ~p", [Id]),
    case mnesia:transaction(fun () -> archive_msg_status(Id) end) of
        {atomic, {error, incomplete}} ->
            ok;
        {atomic, {ok, Counters, ArchRec}} ->
            lists:foreach(fun ({K, V}) ->
                                  _Nv = mnesia:dirty_update_counter({counter, K}, V)
%                                 ,?INFO("Counter ~p now ~p", [K, Nv])
                          end,
                          Counters),
            disk_log:log(State#state.log, ArchRec),
            erms_stats:update_counter(msg_archived);
        {aborted, Reason} ->
            ?ERR("Couldn't archive message, ~p", [Reason])
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info({disk_log, Node, Name, Info}, State) ->
    ?INFO("Msg archive log: ~p (~p) ~p", [Node, Name, Info]),
    {noreply, State};
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State = #state{log=Log}) when is_pid(Log) ->
    ok = disk_log:close(Log),
    terminate(Reason, State#state{log=undefined});
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
archive_msg_status(Id) ->
    {Date, _Time} = calendar:now_to_local_time(erms_uuid:nowtime(Id)),
    archive_msg_status(Date, Id).

archive_msg_status(Date, Id) ->
    [Status] = mnesia:read(msg_status, Id, write),
    archive_msg_status(Date, Id, Status).

archive_msg_status(Date, Id, #msg_status{state=State} = Status) when State == delivered;
                                                                     State == expired ->
    [OrigMsg] = mnesia:read(msg, Status#msg_status.msg_id, write),
    TargetMsgs = case Status#msg_status.targets of 
                     undefined -> [];
                     List when is_list(List) ->
                         [hd(mnesia:read(msg, MsgId, write))
                          || {MsgId,_Dest} <- List,
                             MsgId /= OrigMsg#msg.id]
                 end,
    Msgs = [OrigMsg|TargetMsgs],
    AR = archive_record(Id, Date, Status, Msgs, 
                        mnesia:read(delivery_success, Id, write) ++
                        mnesia:read(delivery_failure, Id, write)),
    lists:foreach(fun (DelMsg) -> ok = mnesia:delete({msg, DelMsg#msg.id}) end,
                  Msgs),
    ok = mnesia:delete({delivery_success, Id}),
    ok = mnesia:delete({delivery_failure, Id}),
    ok = mnesia:delete({msg_status, Id}),
    {ok, erms_reports:delivery_stats({Status, Msgs}), AR};
archive_msg_status(_Date, _Id, Status) ->
    ?WARN("Was asked to archive an incomplete message: ~p", [Status]),
    {error, incomplete}.


archive_record(Id, Date, Status, Msgs, DeliveryInfo) ->
    archive_record(2, Id, Date, Status, Msgs, DeliveryInfo).

archive_record(Version, Id, Date, Status, Msgs, DeliveryInfo) ->
    #archived_msg{version=Version,
                  msg_id=Id,
                  archive_date=Date,
                  msg_status=Status,
                  msgs=Msgs,
                  delivery_details=DeliveryInfo}.


%% A repair is as good 
open_disk_log(Options) ->
    case disk_log:open(Options) of
        {ok, Log} -> {ok, Log};
        {repaired, Log,
         {recovered, Rec}, {badbytes, Bad}} ->
            ?WARN("Message archive was damaged.~n"
                  "Recovered ~p messages. Discarded ~p bad bytes.",
                  [Rec, Bad]),
            {ok, Log};
        Err -> Err
    end.

%% Parameters

param(P) ->
    case application:get_env(erms, msg_archive_log) of
        undefined -> ?WARN("Missing application env: msg_archive_log", []),
                     param(P, undefined);
        {ok, PList} ->
            param(P, PList)
    end.

param(size, undefined) ->
    {?LOGFILESIZE, ?LOGFILECOUNT};
param(size, PropList) ->
    {warn_missing_param(maxbytes, PropList, ?LOGFILESIZE),
     warn_missing_param(maxfiles, PropList, ?LOGFILECOUNT)};
param(file, undefined) ->
    ?LOGFILENAME;
param(file, PropList) ->
    warn_missing_param(file, PropList, ?LOGFILENAME).

warn_missing_param(Name, PList, Default) ->
    case proplists:get_value(Name, PList) of
        undefined ->
            ?WARN("Missing application env data: ~p~nUsing ~p instead.",
                  [{erms, [{msg_archive_log, [{Name, Default}]}]},
                   Default]),
            Default;
        V -> V
    end.

param_test() ->
    ?assertMatch(S when is_list(S), param(file)),
    ?assertMatch({S,C} when (is_integer(S) and is_integer(C)), param(size)).
