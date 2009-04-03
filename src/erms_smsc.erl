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

%%% @doc SMSC Skeleton.
%%%
%%% <p>You may use this skeleton as the starting point to implement your
%%% own SMSCs.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, { 6 Jul 2004} {@time}.
%%% @end
-module(erms_smsc).

-behaviour(gen_smsc).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include_lib("oserl.hrl").
-include_lib("smpp_base.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([start_link/2, stop/1, close_all/1, summarise_state/1,
         update_rx_rules/2, update_rx_rules/3]).

%%%-------------------------------------------------------------------
%%% Internal SMSC exports
%%%-------------------------------------------------------------------
-export([init/1,
         handle_bind/3,
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
-define(SMPP_PORT, ?DEFAULT_SMPP_PORT).
-define(SYSTEM_ID, atom_to_list(?MODULE)).
-define(SMSC_CALL_TIMEOUT, 10 * 1000). % ten seconds

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state}
%%
%% %@doc Representation of the SMSC server state.
%%
%% <dl>
%%   <dt>session_info: </dt><dd>map pid -> coninfo -- used for tx capable sessions</dd>
%%   <dt>con_tbl: </dt><dd>map connectionname -> coninfo -- used for rx capable sessions</dd>
%% </dl>
%% %@end
-record(state, {con_tbl,
                parent,
                name,
                port}).

-record(coninfo, {pid, % key
                  shortcode,
                  name,
                  rx_rules,
                  type,
                  mon_ref}).

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec start_link(Name, Port) -> Result
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server {connection, Name} listening on port Port.
%% @see gen_smsc
%% @end
start_link(Name, Port) when is_integer(Port) ->
    {ok, Pid} = gen_smsc:start_link(?MODULE, [self(), Name, Port], []),
    true = gen_smsc:listen_start(Pid, Port),
    {ok, Pid}.

%% @spec stop(Name) -> ok
%% @doc Stops the SMSC server registered as {connection, Name}.
%% @see handle_call/3
%% @equiv gen_smsc:call(SERVER, die, 10000)
%% @end
stop(Server) ->
    call(Server, die, ?SMSC_CALL_TIMEOUT).

close_all(Server) ->
    call(Server, close_all, ?SMSC_CALL_TIMEOUT).

summarise_state(Server) ->
    call(Server,
         summarise_state, ?SMSC_CALL_TIMEOUT).

update_rx_rules(Server, ConnectionName) ->
    update_rx_rules(Server, ConnectionName,
                    erms_connection:rx_rules(ConnectionName)).

update_rx_rules(Server, ConnectionName, Rules) ->
    call(Server,
         {update_rx_rules, ConnectionName, Rules},
         ?SMSC_CALL_TIMEOUT).

call(Name, Msg, Timeout) when is_list(Name) ->
    gen_smsc:call(erms_connection_mgr:where({connection, Name}),
                  Msg, Timeout);
call({connection, Name}, Msg, Timeout) ->
    gen_smsc:call(erms_connection_mgr:where({connection, Name}), Msg, Timeout);
call(Smsc, Msg, Timeout) when is_pid(Smsc); is_atom(Smsc) ->
    gen_smsc:call(Smsc, Msg, Timeout).

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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#init-1">
%% gen_smsc - init/1</a> callback implementation.
%% 
%% <p>Initiates the server.</p>
%% @end
init([Parent, Name, Port]) when is_pid(Parent) ->
    CTid = ets:new(?MODULE, [private, set, {keypos, 2}]),
    erms_connection_mgr:reg({connection, "SMSC: " ++ Name}),
    % You may start sessions and issue bind requests here.
    {ok, #state{port=Port,
                name=Name,
                con_tbl = CTid,
                parent=Parent}}.


%% @spec handle_bind(Bind, From, State) -> Result
%%    Bind = {CmdName, Session, Pdu}
%%    CmdName = bind_receiver | bind_transmitter | bind_transceiver
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_bind-3">gen_smsc - handle_bind/3</a> callback implementation.
%%
%% <p>Handle <i>bind</i> requests from the peer ESME.</p>
%%
%% <p>The <tt>ParamList</tt> included in the response is used to construct
%% the bind response PDU.  If a command_status other than ESME_ROK is to
%% be returned by the SMSC in the response PDU, the callback should return the
%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%% desired command_status error code.</p>
%% @end
handle_bind({Cmd, SessionPid, Pdu, IpAddr}, _From, State) ->
    {ok, User} = dict:find(system_id, Pdu),
    {ok, Pass} = dict:find(password, Pdu),
    {ok, Version} = dict:find(interface_version, Pdu),
    if
        Version < ?SMPP_VERSION_3_4 -> 
            ?WARN("ESME SMPP version ~p not supported.", [Version]),
            {reply, {error, ?ESME_RBINDFAIL, []}, State};
        true ->
            case erms_auth:authorized(smsc,
                                      User,
                                      Pass) of
                {auth, Shortcode, Connection} ->
                    Rules = erms_connection:rx_rules(Connection),
                    CInfo = #coninfo{pid=SessionPid,
                                     shortcode=Shortcode,
                                     name=Connection,
                                     rx_rules=Rules,
                                     type=Cmd},
                    ?INFO("SMSC Login (~p ~p/~p/~p) ok for ~p.",
                          [IpAddr, User, Pass, Cmd, CInfo]),
                    add_connection(CInfo, State),
                    {reply,
                     {ok, [{system_id, ?SYSTEM_ID}]},
                     State};
                Else ->
                    ?WARN("SMSC login failed (~p ~p/~p ~p) -- ~p.",
                          [IpAddr, User, Pass, Cmd, Else]),
                    {reply, {error, ?ESME_RBINDFAIL, []}, State}
            end
    end.

%% @spec handle_operation(Operation, From, State) -> Result
%%    Operation = {CmdName, Session, Pdu}
%%    CmdName = broadcast_sm        |
%%              cancel_broadcast_sm |
%%              cancel_sm           |
%%              query_broadcast_sm  |
%%              query_sm            |
%%              replace_sm          |
%%              submit_multi        |
%%              submit_sm           |
%%              data_sm
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_operation-3">gen_smsc - handle_operation/3</a> callback implementation.
%%
%% <p>Handle <i>broadcast_sm</i>, <i>cancel_broadcast_sm</i>,
%% <i>cancel_sm</i>, <i>query_broadcast_sm</i>, <i>query_sm</i>,
%% <i>replace_sm</i>, <i>submit_multi</i>, <i>submit_sm</i> and
%% <i>data_sm</i> operations from peer ESMEs.</p>
%%
%% <p>The <tt>ParamList</tt> included in the response is used to construct
%% the response PDU.  If a command_status other than ESME_ROK is to
%% be returned by the SMSC in the response PDU, the callback should return the
%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%% desired command_status error code.</p>
%% @end
handle_operation({submit_sm, Session, Pdu}, _From, State = #state{con_tbl=C}) ->
    [ConInfo] = ets:lookup(C, Session),
    case extract_msg(ConInfo, Pdu) of
        {Ms, M} ->
            case erms_msg_queue:queue(Ms, M) of
                ok -> 
                    {reply, {ok, [{message_id,
                                   smpp_message_id(M)}]}, State};
                {error, _Reason} ->
                    {reply, {error, ?ESME_RX_T_APPN,
                             [{additional_status_info_text,
                               "Messaged discarded - system overload."}]},
                     State}
            end;
        discard ->
            {reply, {error, ?ESME_RX_R_APPN,
                     [{additional_status_info_text,
                       "Message discarded - no valid rx_rule."}]},
             State}
    end;

handle_operation({CmdName, Session, Pdu}, _From, S) ->
    ?WARN("Unexpected operation ~p on session ~p. (~p)",
          [CmdName, Session, dict:to_list(Pdu)]),
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_unbind-3">gen_smsc - handle_unbind/3</a> callback implementation.
%%
%% <p>Handle <i>unbind</i> requests from the peer ESMEs.</p>
%%
%% <p>If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
%% command_status is sent to the MC and the session moves into the unbound
%% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
%% response PDU sent by the session to the MC will have an <tt>Error</tt>
%% command_status and the session will remain on it's current bound state
%% (bound_rx, bound_tx or bound_trx).</p>
%% @end
handle_unbind({unbind, Session, Pdu}, _From, State) -> 
    ?INFO("Session ~p unbound. (~p)", [Session, dict:to_list(Pdu)]),
    remove_connection(Session, State),
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_listen_error-1">gen_smsc - handle_listen_error/1</a> callback implementation.
%%
%% <p>Handle listen failures.</p>
%% @end
handle_listen_error(State = #state{port=Port}) ->
    ?WARN("Listen error, respawning listener.", []),
    Self = self(),
    proc_lib:spawn_link(fun () ->
                                gen_smsc:listen_start(Self, Port),
                                ok
                        end),
    erms_stats:update_counter({?MODULE, listen_error}),
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_call-3">gen_smsc - handle_call/3</a> callback implementation.
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
handle_call({update_rx_rules, CName, Rules}, _From, State) ->
    i_update_rx_rules(CName, Rules, State),
    {reply, {ok, count_connections(State#state.con_tbl, CName)}, State};
handle_call(summarise_state, _From, State) ->
    {reply, {ok, ets:tab2list(State#state.con_tbl)}, State};
handle_call(close_all, _From, State) ->    
    close_all_sessions(State),
    {reply, ok, State};
handle_call(die, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    ?WARN("Unexpected call: ~p", [Request]),
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_cast-2"> gen_smsc - handle_cast/2</a> callback implementation.
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
    ?WARN("Unexpected cast: ~p", [Request]),
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_info-2"> gen_smsc - handle_info/2</a> callback implementation.
%%
%% <p>Handling all non call/cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called.</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_info({'EXIT', Parent, Reason}, State = #state{parent=Parent}) ->
    {stop, Reason, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    ?INFO("Removing Receiver session ~p (~p)", [Pid, Reason]),
    remove_connection(Pid, State),
    {noreply, State};
handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State) ->
    ?INFO("Removing Receiver session ~p (~p)", [Pid, Reason]),
    remove_connection(Pid, State),
    {noreply, State};
handle_info(Info, State) ->
    ?WARN("Unexpected info: ~p", [Info]),
    {noreply, State}.


%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#terminate-2">
%% gen_smsc - terminate/2</a> callback implementation.
%%
%% <p>Shutdown the SMSC server.</p>
%%
%% <p>Return value is ignored by <tt>gen_smsc</tt>.</p>
%% @end
terminate(Reason, State) ->
    if Reason == normal -> ok;
       Reason == shutdown -> ok;
       true -> ?INFO("Terminating: ~p", [Reason])
    end,
    close_all_sessions(State),
    ok.


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#code_change-2"> gen_smsc - code_change/2</a> callback implementation.
%%
%% <p>Convert process state when code is changed.</p>
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_connection(CI = #coninfo{name=Name}, #state{con_tbl=C}) ->
    check_register(C, CI),
    erms_stats:update_counter({?MODULE, con_startup, Name}),
    Monitor = erlang:monitor(process, CI#coninfo.pid),
    ets:insert(C, CI#coninfo{mon_ref=Monitor}).

count_connections(TableId, Name) ->
    ets:select_count(TableId,
                     ets:fun2ms(fun (#coninfo{name=N}) 
                                    when N =:= Name -> true
                                end)).

check_register(TableId, #coninfo{name=ConName}) ->
    check_register(TableId, ConName);
check_register(TableId, ConName) when is_list(ConName) ->
    Count = count_connections(TableId, ConName),
    if 0 =:= Count ->
            ?INFO("Registering connection ~p", [ConName]),
            true = erms_connection_mgr:reg({connection, ConName});
       true -> ok
    end.

check_unregister(TableId, #coninfo{name=ConName}) ->
    Count = count_connections(TableId, ConName),
    if 0 =:= Count ->
            ?INFO("Unregistering connection ~p", [ConName]),
            erms_connection_mgr:unreg({connection, ConName});
       true -> ok
    end.

remove_connection(Pid, State = #state{con_tbl=C}) when is_pid(Pid) ->
    case ets:lookup(C, Pid) of
        [ConInfo] -> remove_connection(ConInfo, State);
        [] -> ?WARN("Tried to remove non-existent connection ~p", [Pid])
    end;
remove_connection(CI = #coninfo{pid=Pid}, #state{con_tbl=C}) ->
    ?INFO("Removing connection ~p (~p).", [CI#coninfo.name, Pid]),
    ets:delete(C, Pid),
    check_unregister(C, CI),
    shutdown_connection(CI).

shutdown_connection(#coninfo{pid=Pid, name=Name}) ->
    proc_lib:spawn(fun () ->
                           erms_stats:update_counter({?MODULE, con_shutdown, Name}),
                           catch(gen_smsc:unbind(Pid)),
                           catch(gen_smsc:session_stop(Pid)),
                           ok
                   end).

i_update_rx_rules(CName, Rules, #state{con_tbl=C}) ->
    ToUpdate = ets:select(C, ets:fun2ms(fun (#coninfo{name=N})
                                            when N =:= CName -> object()
                                        end)),
    ets:select_delete(C, ets:fun2ms(fun (#coninfo{name=N})
                                            when N =:= CName -> true
                                    end)),
    ets:insert(C, lists:map(fun (Ci) ->
                                    Ci#coninfo{rx_rules = Rules}
                            end,
                            ToUpdate)).

close_all_sessions(#state{con_tbl = C}) ->
    ?INFO("Closing all open sessions.", []),
    CIs = ets:tab2list(C),
    % Unregister all
    lists:foreach(fun erms_connection_mgr:unreg/1,
                  lists:usort([{connection, N} || #coninfo{name=N} <- CIs])),
    lists:foreach(fun (CI) ->
                          ets:delete(C, CI#coninfo.pid),
                          shutdown_connection(CI)
                  end,
                  CIs).

extract_msg(#coninfo{name=CName, rx_rules=Rules}, Pdu) ->
    Msg = erms_smpp:pdu_to_msg(Pdu),
    case erms_connection:check_rx_rules(Msg, Rules) of
        discard ->
            ?WARN("Throwing away ~p, no matching receive rule.", [Msg]),
            erms_stats:update_counter({?MODULE, rx_rule_discard, CName}),
            discard;
        {Direction, Shortcode} ->
            %if Shortcode /= S ->            
            %        ?WARN("Shortcode changed by rx_rule, was ~p, now ~p.",
            %              [S, Shortcode]);
            %   true -> ok
            %end,
            {Delivery, Expiry} = erms_smpp:pdu_timing(Pdu),
            MsgStatus = #msg_status{from_connection = CName,
                                    shortcode = Shortcode,
                                    msg_id = Msg#msg.id,
                                    direction = Direction,
                                    expiry_time = Expiry,
                                    delivery_time = Delivery
                                   },
            {MsgStatus, Msg}
    end.

smpp_message_id(#msg{id=ErmsUuid}) ->
    smpp_message_id(erms_uuid:to_list(ErmsUuid));
smpp_message_id(ErmsUuidString) when is_list(ErmsUuidString),
                                     length(ErmsUuidString) > 64 ->
    %?WARN("Message id (~p) length (~p chars) exceeds"
    %      " SMPP spec of 65 (incl. null term).",
    %      [ErmsUuidString, length(ErmsUuidString)]),
    erms_stats:update_counter({?MODULE, message_id_too_long}),
    string:substr(ErmsUuidString, 1, 64);
smpp_message_id(ErmsUuid) when is_list(ErmsUuid) ->
    ErmsUuid.

smpp_message_id_test() ->
    JustRight = string:chars($a, 64),
    TooLong = string:chars($a, 65),
    ReallyTooLong = string:chars($a, 165),
    ?assertMatch(L when L =:= JustRight, smpp_message_id(JustRight)),
    ?assertMatch(L when L =:= JustRight, smpp_message_id(TooLong)),
    ?assertMatch(L when L =:= JustRight, smpp_message_id(ReallyTooLong)).
