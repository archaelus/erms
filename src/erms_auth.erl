%%%-------------------------------------------------------------------
%%%@copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(erms_auth).
-include_lib("eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("qlc.hrl").

%% API
-export([authorized/3,
         valid_login/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (Type, User, Pass) -> {ShortcodeName::string(), ConnectionName::string()}
%%   Type = atom()
%%   User = string()
%%   Pass = string()
%% @doc Checks for a login of type Type for user User with password Pass.
%% @end 

authorized(Type, User, Pass) ->
    F = fun () ->
                mnesia:match_object(#login{type=Type,
                                           username=User,
                                           password=Pass,
                                           _='_'})
        end,
    case mnesia:transaction(F) of
        {atomic, [#login{shortcode_name=Shortcode,
                         connection_name=Connection}]} ->
            erms_stats:update_counter({?MODULE, good_login, Type, User}),
            {auth, Shortcode, Connection};
        _Else ->
            erms_stats:update_counter({?MODULE, bad_login, Type, User}),
            bad_login
    end.

valid_login(L, Shortcodes, Connections) ->
    erms_config:vand([valid_login_userpass(L),
                      valid_login_shortcode(L, Shortcodes),
                      valid_login_connection(L, Connections)]).

valid_login_userpass(#login{username=U, password=P}) ->
    case {lists:flatten(U), lists:flatten(P)} of
        {U, P} -> valid;
        {U, Else} -> {error, {"Invalid password", Else}};
        {Else, P} -> {error, {"Invalid username", Else}}
    end.

valid_login_shortcode(#login{shortcode_name=S}, Shortcodes) ->
    case lists:member(S, Shortcodes) of
        true -> valid;
        false -> {error, {"Missing shortcode", S}}
    end.

valid_login_connection(#login{connection_name=N}, Connections) ->
    case lists:member(N, Connections) of
        true -> valid;
        false -> {error, {"Missing connection", N}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
