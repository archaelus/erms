%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc YAWS appmod for MT message submission
%% @end
%%%-------------------------------------------------------------------
-module(erms_inject_mt).

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
         [{title, [], "Inject MT"}]},
        {body, [],
         {pre, [],
          "Bad Login."}}]}}];
out(Arg, {auth, Shortcode, Connection}) -> 
    {Status, _Message} = find_mt(Shortcode, Connection, Arg),
    [{status, Status},
     {ehtml, []}].

%%====================================================================
%% Internal functions
%%====================================================================

find_mt(S,C,A) ->
    find_mt(S,C,
            queryvar(A,"from"),
            queryvar(A, "to"),
            queryvar(A, "text")).

find_mt(Shortcode,Connection,
        {ok, From}, {ok, To}, {ok, Text}) ->
    Msg = erms_msg:msg(From, To, Text),
    Id = erms_msg_queue:queue_mt(Connection, Shortcode, Msg),
    {200, erms_uuid:to_list(Id)};
find_mt(_,_,undefined,_,_) ->
    {400, "Missing from number"};
find_mt(_,_,_,undefined,_) ->
    {400, "Missing to number"};
find_mt(_,_,_,_,undefined) ->
    {400, "Missing message body"}.

find_login(Arg) ->
    case gregexp:groups(Arg#arg.appmod_prepath, "/\\(.*\\)/") of
        {match, [Login]} ->
            lookup_login(Login, queryvar(Arg, "user"), queryvar(Arg, "pass"));
        _ ->
            bad_login
    end.

lookup_login(_Login, {ok, User}, {ok, Pass}) ->
    erms_auth:authorized(yaws,User,Pass);
lookup_login(_, _, _) ->
    bad_login.
