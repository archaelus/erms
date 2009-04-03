%%%-------------------------------------------------------------------

%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(erms_connection).

-include_lib("eunit/include/eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").

-import(erms_msg, [msg_field/2]).
-import(lists, [foreach/2]).

%% API
-export([lookup/1, name/1, module/1, args/1, rx_rules/1, check_rx_rules/2,
         validate/2]).

-export([sync_deliver/3]).

%%====================================================================
%% API
%%====================================================================

sync_deliver({connection, ConName}, Msg, Timeout) when is_record(Msg, msg) ->
    Pid = erms_connection_mgr:where({connection, ConName}),
    case gen_server:call(Pid, {deliver, Msg}, Timeout) of
        ok ->
            erms_stats:update_counter({delivered, connection, ConName}),
            ok;
        Else -> Else
    end.

lookup(Name) ->
    Query = fun () ->
                    mnesia:read(connection, Name, read)
            end,
    case mnesia:transaction(Query) of
        {atomic, [Con]} -> Con;
        {atomic, []} -> throw({error, no_such_connection});
        Err -> throw({error, Err})
    end.

name(Name) when is_list(Name) ->
    name(lookup(Name));
name(Con) when is_record(Con, connection) ->
    Con#connection.name.

module(Name) when is_list(Name) ->
    module(lookup(Name));
module(Con) when is_record(Con, connection) ->
    Con#connection.mod.

args(Name) when is_list(Name) ->
    args(lookup(Name));
args(Con) when is_record(Con, connection) ->
    Con#connection.args.

rx_rules(Name) when is_list(Name) ->
    rx_rules(lookup(Name));
rx_rules(Con) when is_record(Con, connection) ->
    Con#connection.rules.

check_rx_rules(Msg, [{{Field,Target},{Direction,Shortcode}}|Rules]) ->
    case msg_field(Msg, Field) of
        Match when Match == Target ->
            {Direction, Shortcode};
        NonMatch when NonMatch /= Target ->
            check_rx_rules(Msg, Rules)
    end;
check_rx_rules(_Msg, [{always,{Direction,Shortcode}}|_Rules]) ->
    {Direction, Shortcode};
check_rx_rules(_Msg, []) ->
    discard.

valid_rx_rules(Rules, Shortcodes) ->
    erms_config:vand(lists:map(fun (R) -> valid_rx_rule(R, Shortcodes) end,
                               Rules)).

valid_rx_rule({{Field,Target},{Direction,Shortcode}}, Shortcodes) ->
    erms_config:vand([erms_msg:valid_field(Field),
                      erms_config:flat_string(Target),
                      valid_direction(Direction),
                      valid_shortcode(Shortcode, Shortcodes)]);
valid_rx_rule({always,{Direction,Shortcode}}, Shortcodes) ->
    erms_config:vand([valid_direction(Direction),
                      valid_shortcode(Shortcode, Shortcodes)]).

valid_direction(Dir) -> erms_config:member(Dir, [mt,mo], "Bad direction").

valid_shortcode(Shortcode, Shortcodes) ->
    erms_config:member(Shortcode, Shortcodes, "Missing shortcode").

valid_args(Connection, Mod, Args) ->
    case erms_config:valid_mfa({Mod, valid_args, 1}) of
        valid ->
            Mod:valid_args([Connection|Args]);
        _ -> valid % if there's no arg validation function, assume valid.
    end.

validate(#connection{name=N,rules=RX,mod=M,args=A}, Shortcodes) ->
    erms_config:vand([erms_config:flat_string(N),
                      valid_rx_rules(RX, Shortcodes),
                      erms_config:valid_mfa({M,start_link,length(A)+1}),
                      valid_args(N,M,A)]).

%%====================================================================
%% Internal functions
%%====================================================================
