%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Msg handling functions
%% @end
%%%-------------------------------------------------------------------
-module(erms_msg).

-include_lib("eunit/include/eunit.hrl").
-include_lib("mnesia_model.hrl").

%% API
-export([msg_field/2,
         msg_field/3,
         msg/3,
         msg/4,
         strip_prefix/2,
         clone/1,
         get_msg/1,
         rewrite_field/2,
         translate_to_gsm/2,
         translate_string_to_gsm/1,
         id/1,
         fetch/2,
         valid_field/1
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------

id(#msg{id=Id}) -> Id.

valid_field(Field) ->
    case lists:member(Field, [to,from,text,id]) of
        true -> valid;
        false -> {error, {"Bad message field", Field}}
    end.

msg_field(Msg, to) -> Msg#msg.to;
msg_field(Msg, from) -> Msg#msg.from;
msg_field(Msg, text) -> Msg#msg.text;
msg_field(Msg, id) -> Msg#msg.id.

msg_field(Msg, to, Value) -> Msg#msg{to=Value};
msg_field(Msg, from, Value) -> Msg#msg{from=Value};
msg_field(Msg, text, Value) -> Msg#msg{text=Value};
msg_field(Msg, id, Value) -> Msg#msg{id=Value}.

msg(From, To, Text) ->
    msg(erms_uuid:next(), From, To, Text).

msg(Id, From, To, Text) ->
    #msg{id=Id,
         from=From,
         to=To,
         text=Text}.

get_msg(Id) ->
    Q = fun () -> mnesia:read(msg, Id, read) end,
    {atomic, [Msg]} = mnesia:transaction(Q),
    Msg.

clone(Msg) when is_record(Msg, msg) ->
    Msg#msg{id=erms_uuid:next()}.

strip_prefix(Msg, [Prefix, Field]) ->
    MsgField = msg_field(Msg, Field),
    case lists:prefix(Prefix, MsgField) of
        true ->
            msg_field(clone(Msg), Field, MsgField -- Prefix);
        false ->
            Msg
    end.

rewrite_field(Msg, [Field, Value]) ->
    msg_field(clone(Msg), Field, Value).

rewrite_field_test() ->
    ?assertMatch(#msg{from=foo},
                 rewrite_field(#msg{from=bar}, [from, foo])).

translate_to_gsm(Msg, Fields) ->
    lists:foldl(fun (Field,M) -> translate_field_to_gsm(M, Field) end,
                clone(Msg),
                Fields).

translate_field_to_gsm(Msg, Field) ->
    OldString = msg_field(Msg, Field),
    NewString = translate_string_to_gsm(OldString),
    msg_field(Msg, Field, NewString).

translate_string_to_gsm(Str) ->
    lists:flatten(lists:map(fun (C) -> translate_char_to_gsm(C) end, Str)).

translate_char_to_gsm(16#5b) -> [16#1b, 16#3c]; % [
translate_char_to_gsm(16#5c) -> [16#1b, 16#2f]; % \
translate_char_to_gsm(16#5d) -> [16#1b, 16#3e]; % ]
translate_char_to_gsm(16#5e) -> [16#1b, 16#14]; % ^
translate_char_to_gsm(16#7b) -> [16#1b, 16#28]; % {
translate_char_to_gsm(16#7c) -> [16#1b, 16#40]; % |
translate_char_to_gsm(16#7a) -> [16#1b, 16#29]; % }
translate_char_to_gsm(16#7e) -> [16#1b, 16#3d]; % ~
translate_char_to_gsm(E) -> [E].

fetch(Id, [#msg{id=Id} = M|_Rest]) ->
    M;
fetch(Id, [_Msg|Rest]) ->
    fetch(Id, Rest);
fetch(_, []) ->
    false.

%%====================================================================
%% Internal functions
%%====================================================================

translation_null_test() ->
    ?assert(translate_string_to_gsm("abc") == "abc").

translation_msg_test() ->
    ?assertMatch(#msg{from="abc",to="def"},
                 translate_to_gsm(#msg{from="abc",to="def"}, [])),
    ?assertMatch(#msg{from="abc",to="def"},
                 translate_to_gsm(#msg{from="abc",to="def"}, [from,to])),
    ?assertMatch(#msg{from="abc\e<\e>",to="def"},
                 translate_to_gsm(#msg{from="abc[]",to="def"}, [from])).
    
fetch_test() ->
    ?assert(fetch(foo, [#msg{id=foo},#msg{id=bar}]) == #msg{id=foo}),
    ?assert(fetch(bar, [#msg{id=foo},#msg{id=bar}]) == #msg{id=bar}),
    ?assert(fetch(bobo, [#msg{id=foo},#msg{id=bar}]) == false).
