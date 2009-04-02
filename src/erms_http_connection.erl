%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Generic HTTP Connection server.
%% @end
%%%-------------------------------------------------------------------
-module(erms_http_connection).

-behaviour(gen_server).

-include_lib("eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").

-import(ibrowse_lib, [url_encode/1]).
-import(erms_msg, [get_msg/1]).
-import(erms_connection, [con_deliver_t/1,con_error_t/2,check_dr_t/2]).

%% API
-export([start_link/1,start_link/2,construct_url/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name,
                conf,
                reqs}).

-record(conf, {url,
               method,
               fields}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Name::string()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(ConnectionName) ->
    gen_server:start_link(?MODULE, [ConnectionName], []).

start_link(ConnectionName, Args) ->
    gen_server:start_link(?MODULE, [ConnectionName, Args], []).


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
init([ConnectionName]) ->
    [Conf] = erms_connection:args(ConnectionName),
    init([ConnectionName, Conf]);
init([ConnectionName, Args]) ->
    true = erms_connection_mgr:reg({connection, ConnectionName}),
    {ok, #state{name=ConnectionName,
                conf=Args,
                reqs=dict:new()}}.

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
handle_call({deliver, Msg}, From, State) ->
    begin_deliver(Msg, From, State);
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
handle_info({ibrowse_async_response_end, _}, State) ->
    {noreply, State};
handle_info({ibrowse_async_response, _, _}, State) ->
    {noreply, State};
handle_info({ibrowse_async_headers, ReqId, Code, _Headers}, State) ->
    {noreply, end_deliver(State, ReqId, Code)};
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
terminate(Reason, State) ->
    ?INFO("Unregistering connection ~p because ~p: ~p.",
          [State#state.name,Reason,
           erms_connection_mgr:unreg(State#state.name)]),
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

begin_deliver(Msg, From, State) ->
    Conf = State#state.conf,
    Url = construct_url(Conf#conf.url, Conf#conf.fields, Msg),
    ?INFO("~p GET ~p", [name(State), Url]),
    case ibrowse:send_req(Url, [],
                          Conf#conf.method, [],
                          [{stream_to, self()}]) of
        {ibrowse_req_id, Ref} ->
            {noreply, State#state{reqs=dict:store(Ref, From, State#state.reqs)}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

end_deliver(State, ReqId, Code) ->
    case dict:find(ReqId, State#state.reqs) of
        {ok, From} ->
            end_deliver(State, ReqId, From, Code);
        error ->
            ?WARN("~p HTTP Response for unknown request (~p): ~p", [name(State),
                                                                    ReqId, Code]),
            State
    end.

end_deliver(State, ReqId, From, Code) ->
    case list_to_integer(Code) of
        IntCode when 200 =< IntCode, IntCode =< 299 ->
            gen_server:reply(From, ok);
        Err ->
            gen_server:reply(From, {error, Err})
    end,
    State#state{reqs=dict:erase(ReqId,State#state.reqs)}.

construct_url(Base, Fields, Msg) ->
    lists:flatten(io_lib:fwrite(Base, get_msg_fields(Msg, Fields))).

get_msg_fields(Msg, Fields) ->
    get_msg_fields(Msg, lists:reverse(Fields), []).

get_msg_fields(_Msg, [], SoFar) ->
    SoFar;
get_msg_fields(Msg, [Field|Rest], SoFar) ->
    get_msg_fields(Msg, Rest, [url_encode(get_msg_field(Msg,Field))|SoFar]).

get_msg_field(Msg, {M,F,A}) -> M:F(Msg, A);
get_msg_field(Msg, id) -> erms_uuid:to_list(Msg#msg.id);
get_msg_field(Msg, F) -> erms_msg:msg_field(Msg, F).

construct_url_test() ->
    ?assert(construct_url("http://localhost:13013/cgi-bin/sendsms?username=bob&password=rules&from=~s&to=~s&text=~s",
                          [from,to,text],
                          #msg{from="1234",
                               to="4321",
                               text="test"}) ==
            "http://localhost:13013/cgi-bin/sendsms?username=bob&password=rules&from=1234&to=4321&text=test"),
    ?assert(construct_url("http://localhost:13013/cgi-bin/sendsms?username=bob&password=rules&from=~s&to=~s&text=~s",
                          [from,to,text],
                          #msg{from="1234",
                               to="4321",
                               text="test foo bar baz"}) ==
            "http://localhost:13013/cgi-bin/sendsms?username=bob&password=rules&from=1234&to=4321&text=test+foo+bar+baz").

name(State) ->
    State#state.name.
