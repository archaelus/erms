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

%%% @doc Submit SM example.
%%%
%%% <p>A very simple ESME for submitting short messages.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, {09 Feb 2004} {@time}.
%%% @end
-module(erms_test_esme).

-behaviour(gen_esme).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include_lib("eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("oserl.hrl").
-include_lib("smpp_base.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([shell_start/2,
         connect/2,
         submit_msg/2,
         stop/0,
         test_batch/2]).

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
%%% Macros
%%%-------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, tx_session, host, port, system, password}
%%
%% %@doc Representation of the server's state
%%
%% <dl>
%%   <dt>TxSession: </dt><dd>Pid of the transmitter session.</dd>
%%   <dt>Host: </dt><dd>Hostname of the SMSC.</dd>
%%   <dt>Port: </dt><dd>Tx port of the SMSC.</dd>
%%   <dt>System: </dt><dd>SMPP system id string.</dd>
%%   <dt>Password: </dt><dd>SMPP password string.</dd>
%% </dl>
%% %@end
-record(state, {tx_session, host, port, system, password}).
%-record(msg, {id,
%              from,
%              to,
%              text}).

%%%===================================================================
%%% External functions
%%%===================================================================

%% @spec shell_start(System, Pass) -> {ok, pid()} | {error, term()} | ignore
%%   System = string()
%%   Pass = string()
%% @doc Starts an unlinked esme process that will be used to talk to
%% the SMSC using the System/Pass login/password. The
%% ESME will register itself as erms_test_esme to make other
%% operations easier.
%% @end
shell_start(System, Pass) ->
    gen_esme:start({local, ?SERVER}, ?MODULE, 
                   [System, Pass],
                   []).

%% @spec connect(Host::term(), Port::integer()) -> Response
%%    Response = {ok, Pdu::list()} | {connect_error, Error::term()}
%% @doc Connects the test ESME to the SMSC at Host::Port. Returns the
%% decoded SMPP PDU of the bind_tx command on success and a failure
%% reason if not.
%% @end
connect(Host, Port) ->
    case gen_esme:session_start(?SERVER, Host, Port) of
        {ok, Tx} ->
            gen_esme:call(?SERVER, {new_session, Tx});
        Error ->
            {cannot_connect, Error}
    end.

%% @spec submit_msg(Message::#msg{}, Options::list()) -> ok
%% @doc Submits an erms #msg{} with PDU options Options.
%% @end
submit_msg(Msg = #msg{}, Options) when is_list(Options) ->
    {ok, Session} = gen_esme:call(?SERVER, tx_session, timer:seconds(5)),
    ParamList = erms_smpp:msg_to_pdu(Msg, Options),
    gen_esme_session:submit_sm(Session, ParamList).

%% @spec test_batch(Messages, Count::integer()) -> Errors
%%   Messages = #msg{} | list(#msg{})
%%   Errors = list({MessageNumber::integer(), Message::#msg{}, Error::term})
%% @doc Sends Count messages (from the list Messages) as fast as
%% possible and reports any submission errors.
%% @end
test_batch(Msg = #msg{}, Count)->
    test_batch([Msg], Count);
test_batch(Msgs, Count) when is_integer(Count), is_list(Msgs) ->
    test_batch(Msgs, 1, Count, []).

%% @private
test_batch(_, Iter, Max, Errors) when Iter >= Max ->
    Errors;
test_batch([Msg|Msgs], Iter, Max, Errors) ->
    RotatedMsgs = lists:append(Msgs, [Msg]),
    case submit_msg(Msg, []) of
        {ok, _Pdu} ->
            test_batch(RotatedMsgs, Iter + 1, Max, Errors);
        Error ->
            test_batch(RotatedMsgs, Iter + 1, Max, [{Iter, Msg, Error}|Errors])
    end.

%% @spec stop() -> ok
%%
%% @doc Stops the ESME server.
%%
%% @see handle_call/3
%%
%% @equiv gen_esme:call(SERVER, die, 10000)
%% @end
stop() ->
    gen_esme:call(?SERVER, die, 10000).

%%%===================================================================
%%% Server functions
%%%===================================================================
%% @private
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
init([Host, Port, System, Password]) ->
    {ok, #state{host=Host, port=Port, system=System, password=Password}};
init([System, Password]) ->
    {ok, #state{system=System, password=Password}}.

%% @private
%% @spec handle_outbind(Outbind, From, State) -> Result
%%    OutBind = {outbind, Session, Pdu, IpAddr}
%%    Session = pid()
%%    Pdu = pdu()
%%    IpAddr = {int(), int(), int(), int()}
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
handle_outbind(R = {outbind, _Session, _Pdu, _IpAddr}, _From, State) ->
    ?WARN("Unexpected outbind request ~p", [R]),
    {noreply, State}.

%% @private
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_alert_notification-3">gen_esme - handle_alert_notification/2</a> callback implementation.
%%
%% <p>Handle <i>alert_notification</i> requests from the peer SMSC.</p>
%% @end
handle_alert_notification(R = {alert_notification, _Session, _Pdu}, State) ->
    ?WARN("Unexpected alert_notification request ~p", [R]),
    {noreply, State}.

%% @private
%% @spec handle_enquire_link_failure(EnquireLinkFailure, State) -> Result
%%     EnquireLinkFailure = {enquire_link_failure, Session, CommandStatus}
%%     Session = pid()
%%     CommandStatus = int()
%%     State = term()
%%     Result = {noreply, NewState}               |
%%              {noreply, NewState, Timeout}      |
%%              {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_enquire_link_failure-2">gen_esme - handle_enquire_link_failure/2</a> callback implementation.
%%
%% <p>Notifies when an <i>enquire_link</i> failure occurs (i.e. the SMSC did
%% not respond to our <i>enquire_link</i> operation).</p>
%% @end
handle_enquire_link_failure(R = {enquire_link_failure, _Session, _Status}, State) ->
    ?WARN("Unexpected link failure enqiry request ~p", [R]),
    {noreply, State}.

%% @private
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
handle_operation(R = {_CmdName, _Session, _Pdu}, _From, S) ->
    % Don't know how to handle CmdName
    ?WARN("Don't know how to handle operation ~p", [R]),
    {reply, {error, ?ESME_RINVCMDID, []}, S}.

%% @private
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
    ?INFO("Unbinding from smsc.", []),
    {reply, ok, State}.


%% @private
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
    ?INFO("Need to reconnect - listen error.", []),
    {noreply, State}.

%% @private
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
handle_call(tx_session, _From, S = #state{tx_session=Session}) ->
    {reply, {ok, Session}, S};
handle_call({new_session, Tx}, _From, S = #state{system=System,password=Pass}) ->
    ParamList = [{system_id, System},
                 {password, Pass}],
    case gen_esme:bind_transmitter(Tx, ParamList) of
        {ok, PduResp} ->
            {reply, {ok, PduResp}, S#state{tx_session = Tx}};
        BindError ->
            {stop, BindError, S}
    end;
handle_call({submit_msg, Msg, Options}, _From,
            S = #state{tx_session=Session}) ->
    ParamList = erms_smpp:msg_to_pdu(Msg, Options),
    {reply,
     gen_esme:submit_sm(Session, ParamList),
     S};
handle_call(die, _From, State) ->
    {stop, normal, ok, State}.

%% @private
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
handle_cast(connect, S = #state{tx_session=undefined, host=Host, port=Port}) ->
    Pid = self(),
    F = fun () ->
                case gen_esme:session_start(Pid, Host, Port) of
                    {ok, Tx} ->
                        gen_esme:call(Pid, {new_session, Tx}),
                        ok;
                    Error ->
                        exit({cannot_connect, Error})
                end
        end,
    proc_lib:spawn_link(F),
    {noreply, S};
handle_cast(Request, State) ->
    ?WARN("Unexpected cast: ~p", [Request]),
    {noreply, State}.

%% @private
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
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p~n", [Info]),
    {noreply, State}.

%% @private
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
terminate(kill, _S) ->
    ok;
terminate(Reason, S = #state{tx_session=Pid}) when is_pid(Pid) ->
    catch gen_smsc:unbind(Pid),
    catch gen_smsc:session_stop(Pid),
    terminate(Reason, S#state{tx_session=undefined});
terminate(_Reason, _S) ->
    ok.

%% @private
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
