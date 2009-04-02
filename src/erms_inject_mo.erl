%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc YAWS appmod for MO submission
%% @end
%%%-------------------------------------------------------------------
-module(erms_inject_mo).

-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("yaws_api.hrl").

-import(yaws_api, [queryvar/2]).

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
         [{title, [], "Inject MO"}]},
        {body, [],
         {pre, [],
          "Bad Login."}}]}}];
out(Arg, {auth, Shortcode, Connection}) -> 
    {Status, _Message} = find_mo(Shortcode, Connection, Arg),
    [{status, Status},
     {ehtml, []}].

%%====================================================================
%% Internal functions
%%====================================================================

find_mo(S,C,A) ->
    case queryvar(A, "message") of
        {ok, Value} ->
            find_mo(S,C,
                    queryvar(A, "from"),
                    queryvar(A, "to"),
                    {ok, Value});
        undefined ->
            find_mo(S,C,
                    queryvar(A, "from"),
                    queryvar(A, "to"),
                    queryvar(A, "text"))
    end.

find_mo(Shortcode,Connection,
        {ok, From}, {ok, To}, {ok, Text}) ->
    Msg = erms_msg:msg(From, To, Text),
    Id = erms_msg_queue:queue(Connection, Shortcode, Msg, mo),
    {200, erms_uuid:to_list(Id)};
find_mo(_,_,undefined,_,_) ->
    {400, "Missing from number"};
find_mo(_,_,_,undefined,_) ->
    {400, "Missing to number"};
find_mo(_,_,_,_,undefined) ->
    {400, "Missing message body"}.

find_login(Arg) ->
    User = queryvar(Arg, "user"),
    Pass = queryvar(Arg, "pass"),
    case gregexp:groups(Arg#arg.appmod_prepath, "/\\(.*\\)/") of
        {match, [Login]} when User /= undefined, Pass /= undefined ->
            lookup_login(Login, User, Pass);
        {match, [Login]} ->
            case string:tokens(Arg#arg.pathinfo,"/") of
                [PathUser,PathPass] -> 
                    lookup_login(Login, {ok, PathUser}, {ok, PathPass});
                _Else -> 
                    bad_login
            end;
        _ ->
            bad_login
    end.

lookup_login(_Login, {ok, User}, {ok, Pass}) ->
    erms_auth:authorized(yaws,User,Pass);
lookup_login(_, _, _) ->
    bad_login.
