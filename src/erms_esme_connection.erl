%%% Copyright (C) 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

%%% @doc ESME Skeleton.
%%%
%%% <p>You may use this skeleton as the starting point to implement your
%%% own ESMEs.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, { 6 Jul 2004} {@time}.
%%% @end
-module(erms_esme_connection).

-behaviour(gen_esme).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("oserl.hrl").
-include_lib("smpp_base.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([start_link/1, start_link/2, stop/1, reconnect/1]).

%%%-------------------------------------------------------------------
%%% Internal ESME exports
%%%-------------------------------------------------------------------
-export([init/1,
         handle_outbind/3,
         handle_alert_notification/2,
         handle_enquire_link_failure/2,
         handle_operation/3,
         handle_unbind/3,
         handle_listen_error/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state}
%%
%% %@doc Representation of the ESME server state.
%%
%% <dl>
%%   <dt>: </dt><dd>
%%   </dd>
%% </dl>
%% %@end
-record(state, {conf,
                name,
                session,
                rules,
                listener,
                heartbeatref,
                parent}).

-record(conf, {systemid,
               password,
               smsc,
               type}).

-define(SYSTEM_TYPE, "InternetGW").

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec start_link(Name::string()) -> Result
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the ESME server.
%%
%% @see gen_esme
%% @see start/0
%% @end
start_link(ConnectionName) ->
    start_link(ConnectionName, []).

start_link(ConnectionName, Args) ->
    {ok, Pid} = gen_esme:start_link(?MODULE, [ConnectionName, self(), Args], []),
    Pid ! reconnect,
    {ok, Pid}.
    

%% @spec stop(ConnectionName::string()) -> ok | {error, Reason::term()}
%%
%% @doc Stops the named ESME connection.
%%
%% @see handle_call/3
%%
%% @equiv gen_esme:call(SERVER, die, 10000)
%% @end
stop(ConnectionName) ->
    gen_esme:call(erms_connection_mgr:where({connection, ConnectionName}),
                  die, 10000).

reconnect(ConnectionName) ->
    erms_connection_mgr:where({connection, ConnectionName}) ! reconnect.
    

%%%===================================================================
%%% Server functions
%%%===================================================================
%% @spec init(Args) -> Result
%%    Args    = term()
%%    Result  = {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%%    State   = term()
%%    Timeout = int() | infinity
%%    Reason  = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#init-1">
%% gen_esme - init/1</a> callback implementation.
%% 
%% <p>Initiates the server.</p>
%% @end
init([ConnectionName, Parent]) ->
    [Conf] = erms_connection:args(ConnectionName),
    init([ConnectionName, Parent, Conf]);
init([ConnectionName, Parent, Conf]) ->
    Rules = erms_connection:rx_rules(ConnectionName),
    process_flag(trap_exit, true),
    % You may start sessions and issue bind requests here.
    {ok, TimerRef} = timer:send_interval(timer:minutes(1), reconnect),
    {ok, #state{conf=Conf,
                name=ConnectionName,
                rules=Rules,
                heartbeatref=TimerRef,
                parent=Parent}}.

bind(#conf{type=rx} = Conf, Session) ->
    Params = [{system_id, Conf#conf.systemid},
              {password, Conf#conf.password},
              {system_type, ?SYSTEM_TYPE},
              {interface_version, ?SMPP_VERSION_3_4}],
    gen_esme:bind_receiver(Session,
                           Params);
bind(#conf{type=trx} = Conf, Session) ->
    Params = [{system_id, Conf#conf.systemid},
              {password, Conf#conf.password},
              {system_type, ?SYSTEM_TYPE},
              {interface_version, ?SMPP_VERSION_3_4}],
    gen_esme:bind_transceiver(Session,
                              Params);
bind(#conf{type=tx} = Conf, Session) ->
    Params = [{system_id, Conf#conf.systemid},
              {password, Conf#conf.password},
              {system_type, ?SYSTEM_TYPE},
              {interface_version, ?SMPP_VERSION_3_4}],
    gen_esme:bind_transceiver(Session,
                              Params).
%% @spec handle_outbind(Outbind, From, State) -> Result
%%    OutBind = {outbind, Session, Pdu}
%%    Session = pid()
%%    Pdu = pdu()
%%    From = term()
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_outbind-3">gen_esme - handle_outbind/3</a> callback implementation.
%%
%% <p>Handle <i>oubind</i> requests from the peer SMSC.</p>
%% @end
handle_outbind({outbind, _Session, _Pdu}, _From, State) ->
    ?WARN("Unexpected/wanted/supported outbind.", []),
    {noreply, State}.


%% @spec handle_alert_notification(AlertNotification, State) -> Result
%%    AlertNotification = {alert_notification, Session, Pdu}
%%    Session = pid()
%%    Pdu = pdu()
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_alert_notification-3">gen_esme - handle_alert_notification/3</a> callback implementation.
%%
%% <p>Handle <i>alert_notification</i> requests from the peer SMSC.</p>
%% @end
handle_alert_notification({alert_notification, _Session, _Pdu}, State) -> 
    ?INFO("Unsupported alert notification", []),
    {noreply, State}.

%% @spec handle_enquire_link_failure(EnquireLinkFailure, State) -> Result
%%    EnquireLinkFailure = {enquire_link_failure, Session, CommandStatus}
%%    Session = pid()
%%    CommandStatus = int()
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_alert_notification-3">gen_esme - handle_alert_notification/3</a> callback implementation.
%%
%% <p>Handle <i>alert_notification</i> requests from the peer SMSC.</p>
%% @end
handle_enquire_link_failure({enquire_link_failure, _Session, CommandStatus}, State) -> 
    ?WARN("Link status enquiry failed, cmd status ~p", [CommandStatus]),
    {noreply, State}.


%% @spec handle_operation(Operation, From, State) -> Result
%%    Operation = {deliver_sm, Session, Pdu} | {data_sm, Session, Pdu}
%%    Session = pid()
%%    Pdu = pdu()
%%    From = term()
%%    State = term()
%%    Result = {reply, Reply, NewState}          |
%%             {reply, Reply, NewState, Timeout} |
%%             {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, Reply, NewState}   |
%%             {stop, Reason, NewState}
%%    Reply = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_operation-3">gen_esme - handle_operation/3</a> callback implementation.
%%
%% <p>Handle <i>deliver_sm</i> and <i>data_sm</i> operations (from the peer
%% SMSCs) to the callback ESME.</p>
%%
%% <p>The <tt>ParamList</tt> included in the response is used to construct
%% the response PDU.  If a command_status other than ESME_ROK is to
%% be returned by the ESME in the response PDU, the callback should return the
%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%% desired command_status error code.</p>
%% @end
handle_operation({deliver_sm, _Session, Pdu}, _From, State) ->
    From = dict:fetch(source_addr, Pdu),
    To = dict:fetch(destination_addr, Pdu),
    Text = dict:fetch(short_message, Pdu),
    Msg = erms_msg:msg(From, To, Text),
    case erms_connection:check_rx_rules(Msg, State#state.rules) of
        discard ->
            ?WARN("Throwing away MT, no matching receive rule ~p", [From]);
        {Direction, Shortcode} ->
            Dlr = dict:fetch(registered_delivery, Pdu),
            if Dlr == ?REGISTERED_DELIVERY_MC_NEVER ->
                    ok;
               true ->
                    ?WARN("Unsupported DLR request ~p", [Dlr])
            end,
            _Id = erms_msg_queue:queue(State#state.name, Shortcode, Msg, Direction)
    end,
    {reply, {ok, []}, State};
handle_operation({CmdName, _Session, Pdu}, _From, S) ->
    ?WARN("Unhandled operation, ~p: ~p", [CmdName, dict:to_list(Pdu)]),
    % Don't know how to handle CmdName
    {reply, {error, ?ESME_RINVCMDID, []}, S}.


%% @spec handle_unbind(Unbind, From, State) -> Result
%%    Unbind = {unbind, Session, Pdu}
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {reply, Reply, NewState}          |
%%             {reply, Reply, NewState, Timeout} |
%%             {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, Reply, NewState}   |
%%             {stop, Reason, NewState}
%%    Reply = ok | {error, Error}
%%    Error = int()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_unbind-3">gen_esme - handle_unbind/3</a> callback implementation.
%%
%% <p>Handle <i>unbind</i> requests from the peer SMSC.</p>
%%
%% <p>If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
%% command_status is sent to the MC and the session moves into the unbound
%% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
%% response PDU sent by the session to the MC will have an <tt>Error</tt>
%% command_status and the session will remain on it's current bound state
%% (bound_rx, bound_tx or bound_trx).</p>
%% @end
handle_unbind({unbind, _Session, _Pdu}, _From, State) -> 
    ?INFO("Got unbind request, probably should shutdown.", []),
    {reply, ok, State}.


%% @spec handle_listen_error(State) -> Result
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_listen_error-1">gen_esme - handle_listen_error/1</a> callback implementation.
%%
%% <p>Handle listen failures.</p>
%% @end
handle_listen_error(State) ->
    ?WARN("Listen error.", []),
    {noreply, State}.


%% @spec handle_call(Request, From, State) -> Result
%%    Request   = term()
%%    From      = {pid(), Tag}
%%    State     = term()
%%    Result    = {reply, Reply, NewState}          |
%%                {reply, Reply, NewState, Timeout} |
%%                {noreply, NewState}               |
%%                {noreply, NewState, Timeout}      |
%%                {stop, Reason, Reply, NewState}   |
%%                {stop, Reason, NewState}
%%    Reply     = term()
%%    NewState  = term()
%%    Timeout   = int() | infinity
%%    Reason    = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_call-3">gen_esme - handle_call/3</a> callback implementation.
%%
%% <p>Handling call messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, Reply, NewState}</tt>
%%   terminate/2 is called</li>
%%   <li>On <tt>{stop, Reason, NewState}</tt>
%%   terminate/2 is called</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_call(die, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, From, State) ->
    ?WARN("Unexpected call ~p from ~p", [Request, From]),
    {noreply, State}.


%% @spec handle_cast(Request, State) -> Result
%%    Request  = term()
%%    Result   = {noreply, NewState}          |
%%               {noreply, NewState, Timeout} |
%%               {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout  = int() | infinity
%%    Reason   = normal | term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_cast-2"> gen_esme - handle_cast/2</a> callback implementation.
%%
%% <p>Handling cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called.</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_cast(Request, State) ->
    ?WARN("Unexpected cast ~p", [Request]),
    {noreply, State}.


%% @spec handle_info(Info, State) -> Result
%%    Info     = timeout | term()
%%    State    = term()
%%    Result   = {noreply, NewState}          |
%%               {noreply, NewState, Timeout} |
%%               {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout  = int() | infinity
%%    Reason   = normal | term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_info-2"> gen_esme - handle_info/2</a> callback implementation.
%%
%% <p>Handling all non call/cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called.</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_info({'EXIT', Pid, Reason},
            #state{parent=Pid} = State) ->
    {stop, Reason, State};
handle_info({'EXIT', Pid, _Reason},
            #state{listener=Pid} = State) ->
    {noreply, State#state{listener=undefined}};
handle_info({'EXIT', Pid, _Reason},
            #state{session=Pid} = State) ->
    {noreply, State#state{session=undefined}};
handle_info(reconnect, #state{session=Pid} = State) when is_pid(Pid) ->
    {noreply, State};
handle_info(reconnect, #state{session=undefined,conf=Conf,name=ConnectionName} = State) ->
    {Host, Port} = Conf#conf.smsc,
    case gen_esme:session_start(self(), Host, Port) of
        {ok, Session} ->
            link(Session),
            case bind(Conf, Session) of
                {ok, _Pdu} ->
                    {noreply,
                     State#state{session=Session,
                                 listener=listen(Conf#conf.type, Session, ConnectionName)}};
                BindError ->
                    ?ERR("Couldn't bind session, ~p", [BindError]),
                    catch(gen_esme:session_stop(Session)),
                    {noreply, State}
            end;
        Error ->
            ?ERR("Couldn't connect to the SMSC, ~p", [Error]),
            {noreply, State}
    end;
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.


%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#terminate-2">
%% gen_esme - terminate/2</a> callback implementation.
%%
%% <p>Shutdown the ESME server.</p>
%%
%% <p>Return value is ignored by <tt>gen_esme</tt>.</p>
%% @end
terminate(Reason, #state{session=Session}) when is_pid(Session) ->
    if Reason == normal -> ok;
       Reason == shutdown -> ok;
       true -> ?INFO("Terminating: ~p", [Reason])
    end,
    catch(gen_esme:session_stop(Session)),
    % You may stop sessions and issue unbind requests here.
    ok;
terminate(Reason, _) ->
    if Reason == normal -> ok;
       Reason == shutdown -> ok;
       true -> ?INFO("Terminating: ~p", [Reason])
    end,
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#code_change-2"> gen_esme - code_change/2</a> callback implementation.
%%
%% <p>Convert process state when code is changed.</p>
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

listen(rx, _, _) ->
    undefined;
listen(trx, Session, Name) ->
    listen(tx, Session, Name);
listen(tx, Session, Name) ->
    {ok, Pid} = erms_esme_listener:start_link(Session, Name),
    Pid.
