%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(erms_shortcode).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").
-include_lib("mnesia_model.hrl").

-import(erms_shortcode_lib, [shortcode/1]).
-import(lists, [foreach/2]).

%% API
-export([start_link/1, route/2, reconfigure/1, validate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sc}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Name::string()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server for shortcode Name.
%% @end
%%--------------------------------------------------------------------
start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).


%% @spec route(Status::msg_status(),
%%             Timeout::milliseconds()) -> {ok, NewStatus::msg_status()}
%%                                         | {error,Error}
%% @doc Calls the shortcode Status#msg_status.shortcode to route 
%%  the message with a timeout of Timeout milliseconds.
%% @end
route(#msg_status{shortcode=Shortcode,
                  state = new} = Status, Timeout) ->
    Pid = erms_connection_mgr:where({shortcode, Shortcode}),
    gen_server:call(Pid, {route, Status}, Timeout).


%% @spec reconfigure(Name::string()) -> ok | {error,Error}
%% @doc Causes the shortcode Name to reload its configuration from
%%  mnesia. (Reloads message processing rules)
%% @end
reconfigure(Name) ->    
    gen_server:call(erms_connection_mgr:where({shortcode, Name}), reconfigure).

validate(#shortcode{mt_rules=MT, mo_rules=MO}, Connections) ->
    [{mt, erms_shortcode_lib:validate(MT, Connections)},
     {mo, erms_shortcode_lib:validate(MO, Connections)}].

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
    {atomic, Shortcode} = mnesia:transaction(fun () -> 
                                                     shortcode(Name) 
                                             end),
    true = erms_connection_mgr:reg({shortcode, Name}),
    {ok, #state{sc=Shortcode}}.

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
handle_call(reconfigure, _From, State) ->
    {atomic, Shortcode} = mnesia:transaction(fun () -> 
                                                     shortcode((State#state.sc)#shortcode.name)
                                             end),
    ?INFO("Updated shortcode ~s configuration.", [name(State)]),
    {reply, ok, State#state{sc=Shortcode}};
handle_call({route, Status}, _From, State) when is_record(Status, msg_status) ->
    NewStatusFn = handle_route(Status, State),
    {reply, {ok, NewStatusFn}, State};
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
    (State#state.sc)#shortcode.name.

handle_route(#msg_status{msg_id = MsgId, direction = Direction} = OldStatus, State) ->
    {atomic, [OrigMsg]} = mnesia:transaction(fun () -> mnesia:read(msg, MsgId, read) end),
    fun () -> 
            %% not in a transaction - potentially expensive
            {NewStatus,NewMsgs} = process_msg_rules(Direction, State, OldStatus, OrigMsg), 
            F = fun () ->
                        lists:foreach(fun (Obj) ->
                                              ok = mnesia:write(Obj)
                                      end,
                                      NewMsgs),
                        InProgress = NewStatus#msg_status{state = in_progress},
                        ok = mnesia:write(InProgress),
                        InProgress
                end,
            {atomic, InProgress} = mnesia:transaction(F),
            InProgress
    end.

process_msg_rules(mt, State, MsgStatus, Msg) ->
    Rules = (State#state.sc)#shortcode.mt_rules,
    erms_shortcode_lib:process_rules(MsgStatus, Msg, Rules);
process_msg_rules(mo, State, MsgStatus, Msg) ->
    Rules = (State#state.sc)#shortcode.mo_rules,
    erms_shortcode_lib:process_rules(MsgStatus, Msg, Rules).
