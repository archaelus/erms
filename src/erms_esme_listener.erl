%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ESME transmitter gen_server
%% @end
%%%-------------------------------------------------------------------
-module(erms_esme_listener).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").
-include_lib("oserl.hrl").
-include_lib("smpp_base.hrl").
-include_lib("mnesia_model.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {session, name}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Session::pid(), Name::string()) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Session, Name) ->
    gen_server:start_link(?MODULE, [Session, Name], []).

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
init([Session, Name]) ->
    true = erms_connection_mgr:reg({connection, Name}),
    link(Session),
    {ok, #state{session=Session,
                name=Name}}.

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
handle_call({deliver, Msg}, _From, State) ->
    {reply, deliver(Msg, State), State};
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
terminate(_Reason, State) ->
    erms_connection_mgr:unreg({connection, State#state.name}),
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

deliver(Msg, State) ->
    Params = [{service_type, ?SERVICE_TYPE_NULL},
              {source_addr_ton, ?TON_UNKNOWN},
              {source_addr_npi, ?NPI_ISDN},
              {source_addr, Msg#msg.from},
              {dest_addr_ton, ?TON_UNKNOWN},
              {dest_addr_npi, ?NPI_ISDN},
              {destination_addr, Msg#msg.to},
              {esm_class, ?ESM_CLASS_MODE_STORE_FORWARD},
              {protocol_id, ?PROTOCOL_IDENTIFIER_GSM},
              {priority_flag, ?PRIORITY_FLAG_GSM_SMS_NON_PRIORITY},
              {schedule_delivery_time, ?SCHEDULE_DELIVERY_TIME_IMMEDIATE},
              {validity_period, ?VALIDITY_PERIOD_DEFAULT},
              {registered_delivery, ?REGISTERED_DELIVERY_MC_NEVER},
              {replace_if_present_flag, ?REPLACE_IF_PRESENT_FLAG_DO_NOT_REPLACE},
              {data_coding, ?ENCODING_SCHEME_MC_SPECIFIC},
              {sm_length, length(Msg#msg.text)},
              {short_message, Msg#msg.text}
             ],
    case gen_esme:submit_sm(State#state.session, Params) of
        {ok, DictResp} ->
            {ok, Id} = dict:find(message_id, DictResp),
            ?INFO("External Id ~p -> ~p", [Id, Msg#msg.id]),
            ok;
        Err -> Err
    end.
