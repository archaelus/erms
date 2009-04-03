%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(erms_msg_queue).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0, queue_mt/3,
         queue/2, queue/4, recover/0,
         queue_size/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {msgs, parent, recovery_pid}).
-define(SERVER, ?MODULE).
-define(Q_TIMEOUT, 1000*5).
-define(RESPAWN_RATE, 10). % 10 messages/s
-define(RESPAWN_INTERVAL, round(1000 / ?RESPAWN_RATE)).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).

%% @spec queue_mt(Connection, Shortcode, #msg{}) -> ok | {error, Reason}
%%    Connection = string()
%%    Shortcode = string()
%%    Reason = term()
%% @doc Convenience function for constructing the arguments to queue/2 for an MT..
%% @see queue/2.
%% @end
queue_mt(Connection, Shortcode, Msg) ->
    queue(Connection, Shortcode, Msg, mt).

%% @spec queue(Connection, Shortcode, #msg{}, Direction) -> ok | {error, Reason}
%%    Connection = string()
%%    Shortcode = string()
%%    Direction = mt | mo
%%    Reason = term()
%% @doc Convenience function for constructing the arguments to queue/2
%% @see queue/2.
%% @end
queue(Connection, Shortcode, Msg, Direction) when is_record(Msg, msg), is_list(Connection), is_list(Shortcode) ->
    Status = #msg_status{msg_id = Msg#msg.id,
                         from_connection = Connection,
                         direction = Direction,
                         shortcode = Shortcode,
                         state = new},
    gen_server:call(?SERVER, {queue, Status, Msg}, ?Q_TIMEOUT).

%% @spec queue(#msg_status{}, #msg{}) -> ok | {error, Reason::term()}
%% @doc Submits the Message Status/Message pair for processing and
%% delivery. Message submission entails writing the status/message
%% records to mnesia - this is essentially a fire and forget
%% interface. If an {error,Reason} result is returned, the message
%% can't be queued for some reason (most likely system overload).
%% @end
queue(#msg_status{} = S, #msg{} = M) ->
    gen_server:call(?SERVER, {queue, S, M}, ?Q_TIMEOUT).

recover() ->
    gen_server:cast(?SERVER, recover).

queue_size() ->
    gen_server:call(?SERVER, queue_size).

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
init([Parent]) ->
    process_flag(trap_exit, true),
    erms_stats:poll_self(stats_poll),
    {ok, #state{parent=Parent,
                msgs=dict:new()}}.

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
handle_call(queue_size, _From, State) ->
    {reply, {ok, length(dict:fetch_keys(State#state.msgs))}, State};
handle_call({queue, Status, Msg}, _From, State) ->
    erms_stats:update_counters([msg_enqueue,
                                {msg_enqueue, direction, Status#msg_status.direction},
                                {msg_enqueue, connection, Status#msg_status.from_connection},
                                {msg_enqueue, shortcode, Status#msg_status.shortcode}]),
    try
        MsgStatus = i_queue(Status, Msg),
        NewState = i_process(MsgStatus, State),
        {reply, ok, NewState}
    catch
        Error ->
            erms_stats:update_counter({?MODULE, queue_error}),
            {reply, {error, Error}, State}
    end;
handle_call({respawn, MsgStatus}, _From, State) ->
    {reply, ok, i_process(MsgStatus, State)};
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
handle_cast(recover, #state{recovery_pid=undefined} = State) ->
    Pid = do_recovery(),
    ?INFO("Recovery request (new recovery process ~p)", [Pid]),
    {noreply, State#state{recovery_pid=Pid}};
handle_cast(recover, #state{recovery_pid=Pid} = State) when is_pid(Pid) ->
    ?INFO("Recovery request (existing recovery process ~p)", [Pid]),
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
handle_info(stats_poll, State) ->
    erms_stats:update_rate({?MODULE, queue_size},
                           length(dict:fetch_keys(State#state.msgs))),
    {noreply, State};
handle_info({'EXIT', Pid, normal}, #state{recovery_pid=Pid} = State) ->
    {noreply, State#state{recovery_pid=undefined}};
handle_info({'EXIT', Pid, _Error}, #state{recovery_pid=Pid} = State) ->
    % recovery pid died unexpectedly - respawn it. Is this a good idea? (not rhetorical :)
    {noreply, State#state{recovery_pid=do_recovery()}};
handle_info({'EXIT', Parent, Reason}, #state{parent=Parent} = State) ->
    {stop, Reason, State};
handle_info({'EXIT', Pid, Reason}, #state{msgs=Msgs} = State) ->
    case dict:find(Pid, Msgs) of
        {ok, MsgId} when Reason == normal ->
            erms_msg_archiver:archive(MsgId),
            {noreply, State#state{msgs=dict:erase(Pid, Msgs)}};
        {ok, MsgId} ->
            erms_msg_queue_processor:record_failure(MsgId, unknown, unknown, Reason),
            erms_stats:update_counter({erms_msg_queue_processor, exit, Reason}),
            ?INFO("Processor exited with reason ~p", [Reason]),
            {noreply, State#state{msgs=dict:erase(Pid, Msgs)}};
        _ ->
            ?WARN("Unknown process exit ~p: ~p", [Pid, normal]),
            {noreply, State}
    end;
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

i_queue(Status, Msg) ->
    {atomic, ok} = 
        mnesia:transaction(fun () ->
                                   ok = mnesia:write(Msg),
                                   ok = mnesia:write(Status),
                                   ok
                           end),
    Status.

i_process(#msg_status{msg_id=Id} = Status, #state{msgs=Msgs} = State) ->
    case erms_msg_queue_processor:start_link(Status) of
        {ok, Pid} when is_pid(Pid) ->
            State#state{msgs = dict:store(Pid, Id, Msgs)};
        {ignore, _Pid} ->
            State
    end.

do_recovery() ->
    MS = ets:fun2ms(fun (#msg_status{state=new}) -> object();
                        (#msg_status{state=in_progress}) -> object()
                    end),
    {atomic, Incomplete} = mnesia:transaction(fun mnesia:select/2,
                                              [msg_status, MS]),
    R = fun () ->
                lists:foreach(fun (S) ->
                                      timer:sleep(?RESPAWN_INTERVAL),
                                      gen_server:call(?SERVER, {respawn, S})
                              end,
                              Incomplete),
                ?INFO("Recovery complete.", [])
        end,
    proc_lib:spawn_link(R).
