%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Vik Olliver <vik@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc YAWS appmod for MO/MT generic submission
%% @end
%%%-------------------------------------------------------------------
-module(erms_inject).

-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("yaws_api.hrl").

%% API
-export([out/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------

out(Arg) when is_record(Arg, arg) ->
    case (Arg#arg.req)#http_request.method of
        'HEAD' -> [{status, 200},{ehtml,[]}];
        _ -> out(Arg, find_login(Arg))
    end.

out(_Arg, bad_login) -> 
    [{status, 403},
     {ehtml,
      {html, [],
       [{head, [],
         [{title, [], "Inject SMS"}]},
        {body, [],
         {pre, [],
          "Bad Login."}}]}}];
out(Arg, {auth, _Shortcode, Connection}) ->
    auth_out(Arg, request_msg(Arg, Connection)).

auth_out(Arg, {MS = #msg_status{}, M = #msg{}}) ->
    sendmsg_out(Arg, erms_msg_queue:queue(MS, M));
auth_out(_Arg, {error, Code, Message}) ->
    [{status, Code},
     {ehtml, [Message]}].

sendmsg_out(_Arg, ok) ->
    [{status, 200},
     {ehtml, []}];
sendmsg_out(_Arg, Error) ->
    ?ERR("Couldn't queue message, ~p", [Error]),
    [{status, 503},
     {ehtml, ["Message submission failed."]}].

%%====================================================================
%% Internal functions
%%====================================================================
% Turn the MCC/MNC tuple into a network
network_from_mcc("530-01") ->
    vodafone;
network_from_mcc("530-02") ->
    telecom;
network_from_mcc(_) ->
    undefined.


request_msg(Arg, Connection) ->
    % Might be used to add to cache, certainly will be used later.
    Dest=queryvar(["to","destination","destination_addr"], Arg),
    % If we have a network specified, cache that with the destination.
    case queryvar(["network"],Arg) of
        {ok,Network} ->
            erms_mnp_cache:cache_result(Dest,network_from_mcc(Network));
        undefined ->
            % Do nuthin.
            []
        end,

    case request_msg(queryvar(["from","source","source_addr"], Arg),
                     Dest,
                     queryvar(["text","message"], Arg)) of
        Msg = #msg{} ->
            Rules = erms_connection:rx_rules(Connection),
            case erms_connection:check_rx_rules(Msg, Rules) of
                discard -> {error, 400, "Cannot determine message direction from connection rules."};
                {Direction, Shortcode} ->
                    {request_msg_status(Arg, Connection, Direction, Shortcode, Msg),
                     Msg}
            end;
        {error, Reason} ->
            {error, 400, Reason}
    end.

request_msg_status(Arg, Connection, Direction, Shortcode, Msg) ->
    #msg_status{from_connection = Connection,
                shortcode = Shortcode,
                msg_id = Msg#msg.id,
                direction = Direction,
                expiry_time = request_validity(Arg),
                delivery_time = request_delivery_time(Arg)
               }.

request_msg(undefined, _, _) ->
    {error, "Missing From address parameter."};
request_msg(_, undefined, _) ->
    {error, "Missing To address parameter."};
request_msg(_, _, undefined) ->
    {error, "Missing message Text parameter."};
request_msg({ok, From}, {ok, To}, {ok, Text}) ->
    erms_msg:msg(From, To, Text).

request_validity(Arg) ->
    case queryvar(["validity_period", "validity"], Arg) of
        {ok, Value} -> erms_smpp:smpptime_to_universal(Value);
        undefined -> undefined
    end.

request_delivery_time(Arg) ->
    case queryvar(["scheduled_delivery", "validity"], Arg) of
        {ok, Value} -> erms_smpp:smpptime_to_universal(Value);
        undefined -> undefined
    end.

find_login(Arg) ->
    User = queryvar(["user"], Arg),
    Pass = queryvar(["pass","password"], Arg),
    case gregexp:groups(Arg#arg.appmod_prepath, "/\\(.*\\)/") of
        {match, [Login]} when Arg#arg.pathinfo =/= undefined ->
            case string:tokens(Arg#arg.pathinfo,"/") of
                [PathUser,PathPass] -> 
                    lookup_login(Login, {ok, PathUser}, {ok, PathPass});
                _Else -> 
                    lookup_login(Login, User, Pass)
            end;
        {match, [Login]} -> lookup_login(Login, User, Pass);
        _ ->
            bad_login
    end.

lookup_login(_Login, {ok, User}, {ok, Pass}) ->
    erms_auth:authorized(yaws,User,Pass);
lookup_login(_, _, _) ->
    bad_login.

queryvar([] , _) -> undefined;
queryvar([Var|Vars], Arg = #arg{}) ->
    case yaws_api:queryvar(Arg, Var) of
        {ok, Value} ->
            {ok, Value};
        undefined ->
            queryvar(Vars, Arg)
    end.

