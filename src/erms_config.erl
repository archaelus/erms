%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(erms_config).

%% API
-export([flat_string/1,
         vand/1,
         member/3,
         valid_mfa/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% spec flat_string([List]) -> 
%% doc 
%% end 

flat_string(L) when is_list(L) ->
    case lists:flatten(L) of
        L -> valid;
        Else -> {error, {"Not a flat string", Else}}
    end;
flat_string(Other) ->
    {error, {"Not a flat string", Other}}.

vand(List) ->
    case lists:filter(fun (valid) -> false; (_Error) -> true end,
                      lists:flatten(List)) of
        [] -> valid;
        Errors -> Errors
    end.

member(Item, Items, Msg) ->
    case lists:member(Item, Items) of
        true -> valid;
        false -> {error, {Msg, Item}}
    end.

valid_mfa({Module, Function, Args}) when is_list(Args) ->
    valid_mfa({Module, Function, length(Args)});
valid_mfa({Module, Function, Args}) when is_integer(Args) ->
    case lists:member({Function, Args}, Module:module_info(exports)) of
        false -> {error, {"Missing function", {Module, Function, Args}}};
        true -> valid
    end.

%%====================================================================
%% Internal functions
%%====================================================================
