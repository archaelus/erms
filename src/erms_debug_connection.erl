%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(erms_debug_connection).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("mnesia_model.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(erms_connection, [con_deliver_t/1,con_error_t/2,check_dr_t/2]).
-import(erms_msg, [get_msg/1]).

-record(state, {conf}).
-record(conf, {name}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Name::string()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

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
init([Name]) ->
    Conf = #conf{name=Name},
    true = erms_connection_mgr:reg({connection, Name}),
    {ok, #state{conf=Conf}}.

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
handle_call({deliver, Msg}, _From, State) when is_record(Msg, msg) ->
    {reply, deliver(Msg, name(State)), State};
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

deliver(Msg, Name) ->
    T1 = erms_uuid:nowtime(Msg#msg.id),
    TDiff = timer:now_diff(erlang:now(), T1) div 1000,
    ?INFO("~p (~p ms) - Message from ~p to ~p: ~p",
          [Name, TDiff, Msg#msg.from, Msg#msg.to, Msg#msg.text]),
    ok.
