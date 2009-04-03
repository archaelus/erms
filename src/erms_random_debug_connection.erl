%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(erms_random_debug_connection).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("mnesia_model.hrl").

%% API
-export([start_link/1, start_link/2, block/1, mode/2,
         adjust_n/2, block_for/2, drop/2, normal/1, block/2,
         report/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(erms_connection, [con_deliver_t/1,con_error_t/2,check_dr_t/2]).
-import(erms_msg, [get_msg/1]).

-record(state, {conf, n = 1, seen,
                mode = normal,
                report = true}).
-record(conf, {name}).

% modes normal | block_n | drop_n | blocked

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Name::string()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
    start_link(Name, []).
start_link(Name, Args) ->
    gen_server:start_link(?MODULE, [Name|Args], []).

report({connection, Name}, ReportP) ->
    report(erms_connection_mgr:where({connection, Name}), ReportP);
report(Pid, ReportP) when is_boolean(ReportP) ->
    gen_server:call(Pid, {report, ReportP}).

% Blocks all messages
block(Server) ->
    mode(Server, blocked).

% Blocks messages for the first N attempts.
block(Server, N) ->
    ok = adjust_n(Server, N),
    mode(Server, block_n).    

% Drops messages 1 in N times.
drop(Server, N) ->
    ok = adjust_n(Server, N),
    mode(Server, drop_n).

% Delivers all messages
normal(Server) ->
    mode(Server, normal).

mode({connection, Name}, State) ->
    mode(erms_connection_mgr:where({connection, Name}), State);
mode(Server, State) ->
    gen_server:call(Server, {adjust_mode, State}).

% Blocks all messages for Time ms.
block_for(Server, Time) ->
    spawn(fun () ->
                  ?INFO("Blocking ~p for ~p ms.", [Server, Time]),
                  mode(Server, blocked),
                  timer:sleep(Time),
                  mode(Server, normal),
                  ?INFO("Unblocking ~p", [])
          end).

% Adjusts N in any mode.
adjust_n({connection, Name}, N) ->
    adjust_n(erms_connection_mgr:where({connection, Name}), N);
adjust_n(Server, N) ->
    gen_server:call(Server, {adjust_n, N}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%%  Args = list()
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([Name]) ->
    Conf = #conf{name=Name},
    true = erms_connection_mgr:reg({connection, Name}),
    random:seed(),
    {ok, #state{conf=Conf,
                seen=dict:new()}}.

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
handle_call({report, true}, _From, State) ->
    {reply, {ok, reporting}, State#state{report=true}};
handle_call({report, false}, _From, State) ->
    {reply, {ok, not_reporting}, State#state{report=false}};
handle_call({adjust_n, N}, _From, State) ->
    {reply, ok, State#state{n = N}};
handle_call({adjust_mode, Mode}, _From, State) ->
    {reply, ok, State#state{mode=Mode}};
handle_call({deliver, _Msg}, _From, #state{mode=blocked} = State) ->
    {reply, {error, blocked}, State};
handle_call({deliver, Msg}, _From, #state{mode=normal} = State) when is_record(Msg, msg) ->
    i_report(Msg, State),
    {reply, ok, State};
handle_call({deliver, Msg}, _From, #state{mode=drop_n} = State) when is_record(Msg, msg) ->
    R = case random:uniform(State#state.n) of
            1 when State#state.n > 1 -> {error, random_drop};
            _ -> 
                i_report(Msg, State),
                ok
        end,
    {reply, R, State};
handle_call({deliver, Msg}, _From, #state{mode=block_n,n=N,seen=D} = State) when is_record(Msg, msg) ->
    Id = erms_msg:id(Msg),
    case dict:find(Id, D) of
        error ->
            {reply, {error, blocked}, State#state{seen=dict:update_counter(Id,1,D)}};
        {ok, M_N} when M_N < N ->
            {reply, {error, blocked}, State#state{seen=dict:update_counter(Id,1,D)}};
        {ok, _} ->
            i_report(Msg, State),
            {reply, ok, State#state{seen=dict:erase(Id,D)}}
    end;
handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p in state ~p.", [Call, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
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

name(State) ->
    (State#state.conf)#conf.name.

i_report(Msg, State = #state{report=true}) ->
    T1 = erms_uuid:nowtime(Msg#msg.id),
    TDiff = timer:now_diff(erlang:now(), T1),
    ?INFO("~p (~p ms) - Message from ~p to ~p: ~p",
          [name(State), TDiff / 1000, 
           Msg#msg.from, Msg#msg.to, Msg#msg.text]);
i_report(_Msg, _State) ->
    ok.
