%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Shortcode rule processing library
%% @end
%%%-------------------------------------------------------------------
-module(erms_shortcode_lib).

-include_lib("eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").

-import(erms_msg, [msg_field/2]).

%% API
-export([mt_rules/1, process_rules/3, shortcode/1, test_filter/2,
         mo_rules/1, process_rule/2,
         validate/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------

%% The rule processing state data type. #msg_status{}, new #msg{}s,
%% current #msg{}
-record(pstate, {ms,
                 msgs = [],
                 current}).

process_rules(S = #msg_status{}, M = #msg{}, Rules) ->
    Result = process_rules(#pstate{ms=S, current=M, msgs=[]}, Rules ++ [end_of_rules]),
    {Result#pstate.ms, lists:usort(Result#pstate.msgs)}.

process_rules(S = #pstate{}, []) ->
    S;
process_rules(S = #pstate{}, [Rule|Rules]) ->
    case process_rule(Rule, S) of
        return ->
            S;
        continue ->
            process_rules(S, Rules);
        {continue, NewState = #pstate{}} ->
            process_rules(NewState, Rules);
        {finish, NewState = #pstate{}} ->
            NewState;
        {finish, NewState, NewRules} ->
            process_rules(NewState, NewRules)
    end.

process_rule(end_of_rules, _) ->
    throw(end_of_rules);

process_rule(return, _PState) ->
    return;

process_rule({delay, Milliseconds}, _PState) ->
    timer:sleep(Milliseconds),
    continue;

process_rule({filter, Module, Function, Args}, S = #pstate{current=Msg}) ->
    NewMsg = Module:Function(Msg, Args),
    {continue, S#pstate{current=NewMsg}};

process_rule({mnp, Field, Rules}, S = #pstate{current=Msg}) ->
    Net = erms_mnp:mnp_lookup(msg_field(Msg, Field), [standard_meta_mnp]),
    handle_mnp_result(S, Net, Rules);
process_rule({mnp, Options, Field, Rules}, S = #pstate{current=Msg}) ->
    Net = erms_mnp:mnp_lookup(msg_field(Msg, Field), Options),
    handle_mnp_result(S, Net, Rules);

process_rule({match, Field, Target, Dest}, S = #pstate{current=Msg}) ->
    case msg_field(Msg, Field) of
        MsgField when MsgField =:= Target, is_list(Dest) ->
            {finish, S, Dest};
        MsgField when MsgField =:= Target ->
            {finish, add_target(S, Msg, Dest)};
        _NoMatch ->
            continue
    end;
process_rule({match_re, Field, Regexp, Dest}, S = #pstate{current=Msg}) ->
    case regexp:first_match(msg_field(Msg, Field), Regexp) of
        {match, _Start, _Length} when is_list(Dest) ->
            {finish, S, Dest};
        {match, _Start, _Length} ->
            {finish, add_target(S, Msg, Dest)};
        _NoMatch ->
            continue
    end;

process_rule({connection, Connection}, S = #pstate{current=Msg}) ->
    {finish, add_target(S, Msg, {connection, Connection})};

process_rule({reply, Term, ReplyRules}, S = #pstate{current=Msg}) ->
    NewMsg = gen_reply(Msg, Term),
    NewState = process_rules(S#pstate{current=NewMsg}, ReplyRules),
    {continue, NewState#pstate{current=Msg}};
process_rule({reply, From, To, Text, ReplyRules}, S = #pstate{current=Msg}) ->
    NewMsg = gen_reply(Msg, From, To, Text),
    NewState = process_rules(S#pstate{current=NewMsg}, ReplyRules),
    {continue, NewState#pstate{current=Msg}}.

gen_reply(Msg, {M,F,A}) ->
    M:F(Msg, A);
gen_reply(Msg, Text) ->
    gen_reply(Msg, {msg_field, to}, {msg_field, from}, Text).
gen_reply(Msg, {msg_field, Field}, To, Text) ->
    gen_reply(Msg, msg_field(Msg, Field), To, Text);
gen_reply(Msg, From, {msg_field, Field}, Text) ->
    gen_reply(Msg, From, msg_field(Msg, Field), Text);
gen_reply(_Msg, From, To, Text) ->
    erms_msg:msg(From, To, Text).

handle_mnp_result(S = #pstate{current=Msg}, Net, Rules) ->
    case lists:keysearch(Net, 1, Rules) of
        {value, {_N, NetRules}} when is_list(NetRules) ->
            {finish, S, NetRules};
        {value, {_N, {connection, Connection}}} ->
            {finish, add_target(S, Msg, {connection, Connection})};
        false ->
            continue
    end.

mt_rules(Name) ->
    (shortcode(Name))#shortcode.mt_rules.

mo_rules(Name) ->
    (shortcode(Name))#shortcode.mo_rules.

shortcode(Name) ->
    [Shortcode] = mnesia:read(shortcode, Name, read),
    Shortcode.

validate([], _Connections) ->
    valid;
validate([Rule|Rules], Connections) ->
    case valid(Rule, Connections) of
        valid -> validate(Rules, Connections);
        Else -> Else
    end.

valid(Dest = {connection, _}, Connections) ->
    valid_dest(Dest, Connections);
valid({delay, Milliseconds}, _) when is_integer(Milliseconds) -> valid;
valid({delay, T}, _) -> {error, {"{delay, Time} must be an integer number of milliseconds", T}};
valid(return, _) -> valid;
valid({filter, Module, Function, _Args}, _) ->
    erms_config:valid_mfa({Module, Function, 2});
valid({mnp, Field, Rules}, Connections) ->
    erms_config:vand([erms_msg:valid_field(Field),
                      validate_mnp_rules(Rules, Connections)]);
valid({mnp, Options, Field, Rules}, Connections) ->
    erms_config:vand([erms_mnp:valid_options(Options),
                      valid({mnp, Field, Rules}, Connections)]);
valid({match, Field, _Target, Dest}, Connections) ->
    erms_config:vand([erms_msg:valid_field(Field),
                      valid_dest(Dest, Connections)]);
valid({match_re, Field, Regexp, Dest}, Connections) ->
    case regexp:parse(Regexp) of
        {ok, _} when is_tuple(Dest) ->
            erms_config:vand([erms_msg:valid_field(Field),
                              valid_dest(Dest, Connections)]);
        {ok, _} when is_list(Dest) ->
            erms_config:vand([erms_msg:valid_field(Field),
                              validate(Dest, Connections)]);
        Else -> {error, {"Invalid regular expression.", Else}}
    end;
valid({reply, Term, ReplyRules}, Connections) ->
    erms_config:vand([valid_reply(Term),
                      validate(ReplyRules, Connections)]);
valid({reply, From, To, Text, ReplyRules}, Connections) ->
    erms_config:vand([valid_reply(From, To, Text),
                      validate(ReplyRules, Connections)]);
valid(Rule, _) -> {error, {"Bad rule", Rule}}.

validate_mnp_rules(NetRules, Connections) ->
    Validate_mnp_net_rule = fun ({Net, Rules}) ->
                                    case validate(Rules, Connections) of
                                        valid -> valid;
                                        Else ->
                                            {error,
                                             {"Net rule error", {Net, Else}}}
                                    end
                            end,
    erms_config:vand(lists:map(Validate_mnp_net_rule, NetRules)).

valid_reply({M,F,_A}) ->
    erms_config:valid_mfa({M,F,2});
valid_reply(String) ->
    erms_config:flat_string(String).

valid_reply(From, To, Text) ->
    erms_config:vand([valid_reply_field(From),
                      valid_reply_field(To),
                      valid_reply_field(Text)]).

valid_reply_field({msg_field, Field}) ->
    erms_msg:valid_field(Field);
valid_reply_field(String) ->
    erms_config:flat_string(String).

valid_dest({connection, C}, Connections) ->
    case lists:member(C, Connections) of
        false -> {error, {"Missing connection", C}};
        _ -> valid
    end;
valid_dest(D, _) -> 
    {error, {"Unrecoginized destination", D}}.

%%====================================================================
%% Internal functions
%%====================================================================

add_target(S = #pstate{ms=Ms,msgs=Msgs}, Msg, Dest) ->
    S#pstate{ms=Ms#msg_status{targets=[{Msg#msg.id, Dest}|
                                       Ms#msg_status.targets]},
             msgs=[Msg|Msgs]}.

rule_match_test() ->
    M = #msg{id=test,from="018"},
    PS1 = #pstate{ms=#msg_status{},
                  current=M,
                  msgs=[]},
    ?assertMatch(#pstate{msgs=[M],
                         current=M,
                         ms=#msg_status{targets=[{test, dest}]}},
                 process_rules(PS1, [{match, from, "018", dest}])
                ).

rule_match_re_test() ->
    PS1 = #pstate{ms=#msg_status{},
                  msgs=[]},
    M1 = #msg{id=test,to="027123456"},
                                                %M2 = #msg{id=test,to="021123456"},
    ?assertMatch(#pstate{ms=#msg_status{targets=[{test, {dest}}]}},
                 process_rules(PS1#pstate{current=M1},
                               [{match_re, to, "^(\\+?64|0)?2(5|7)", {dest}}])),
    ?assertMatch(#pstate{msgs=[M1], ms=#msg_status{targets=[{test, {connection, "somewhere"}}]}},
                 process_rules(PS1#pstate{current=M1},
                               [{match_re, to, "^(\\+?64|0)?2(5|7)",
                                 {connection, "somewhere"}}
                               ]
                              )),
    ?assertMatch(#pstate{msgs=[]},
                 process_rules(PS1#pstate{current=M1},
                               [{match_re, to, "^(\\+?64|0)?2(1|9)",
                                 [{connection, "somewhere"}]}])).

rule_mnp_test() ->
    {ok, _Pid} = erms_mnp_cache:start(),
    PS1 = #pstate{ms=#msg_status{},
                  msgs=[]},
    VfMsg = #msg{to="021767390", from="test", id=test, text="foo"},
    Objects = process_rule({mnp, to, [{telecom, {connection, error}},
                                      {vodafone, {connection, "Vodafone"}}]},
                           PS1#pstate{current=VfMsg}),
    ?assertMatch({finish, #pstate{ms=#msg_status{targets=[{test, {connection, "Vodafone"}}]}}},
                 Objects).

%% rule_mnp2_test() ->
%%     TMsg = #msg{to="021 221 6370", from="test", id=tcom, text="foo"},
%%     Ob2 = process_rule(test_mt_id,
%%                        {mnp, to, [{telecom, [{filter, erms_numbers, msg_filter, [e123,to]},
%%                                              {connection, "Telecom"}]},
%%                                   {vodafone, {connection, "Vodafone"}}]},
%%                        TMsg),
%%     ?assertMatch([#msg{to="0212216370", from="test"},
%%                   {target, _, {connection, "Telecom"}}],
%%                  Ob2).

%% rule_mnp3_test() ->
%%     Ob2 = process_rules(test_mt_id,
%%                         [{mnp, [], text, [{invalid, [{connection, invalid}]},
%%                                           {telecom, [{connection, telecom}]},
%%                                           {not_telecom, [{connection, vodafone}]},
%%                                           {error, [{connection, telecom}]}
%%                                          ]}],
%%                         #msg{id=test,text="Is 021767390 a vodafone number?"}),
%%     ?assert(lists:any(fun ({target, test, {connection, vodafone}}) -> true; (_) -> false end, Ob2)),
%%     ?assert(lists:any(fun (#msg{id=test,text="Is 021767390 a vodafone number?"}) -> true; (_) -> false end, Ob2)).


delay_test() ->
    {Time,Value} = timer:tc(?MODULE,
                            process_rule,
                            [{delay, 1000},
                             #pstate{ms=#msg_status{},
                                     current=#msg{id=test}}]),
    ?assert(Time >= 1000),
    ?assertMatch(continue, Value).

reply_test() ->
    ?assertMatch({continue,
                  #pstate{ms=#msg_status{targets=[{_, {connection, test_con}}]},
                          msgs=[#msg{from="From", to="To", text="Text"}]}},
                 process_rule({reply, "From", "To", "Text", [{connection, test_con}]},
                              #pstate{ms=#msg_status{},
                                      current=#msg{id=test}})),
    #pstate{ms=#msg_status{targets=T},
            msgs=M}
        = process_rules(#pstate{ms=#msg_status{},
                                current=#msg{id=test1}},
                        [{reply, "From", "To", "Text", [{connection, test_con}]},
                         {connection, other_con}]),
                                                %    ?INFO("targets:~p~nmsgs:~p", [T, M]),
    ?assert(lists:any(fun (#msg{id=test1}) ->
                              true;
                          (_) -> false
                      end, M)),
    ?assert(lists:any(fun (#msg{text="Text"}) ->
                              true;
                          (_) -> false
                      end, M)),
    ?assert(lists:any(fun ({_, {connection, test_con}}) ->
                              true;
                          (_) -> false
                      end, T)),
    ?assert(lists:any(fun ({test1, {connection, other_con}}) ->
                              true;
                          (_) -> false
                      end, T)).

%% return_test() ->
%%     Ob2 = process_rules(test_mt_id,
%%                         [{mnp, from, [{vodafone, [{reply, "From", "To", "Text", [{connection, test_con}]}, return]}]},
%%                          {connection, other_test_con}],
%%                         #msg{id=test, from="021767390"}),
%%     ?assert(lists:member(#msg{id=test, from="021767390"}, Ob2)),
%%     ?assert(lists:member({target, test, {connection, other_test_con}}, Ob2)),
%%     ?assert(lists:any(fun ({target, _, {connection, test_con}}) -> true; (_) -> false end, Ob2)),
%%     ?assert(lists:any(fun (#msg{from="From",to="To",text="Text"}) -> true; (_) -> false end, Ob2)).

                                                % Reverses the direction of a #msg{} (swap from and to numbers).
test_filter(Msg, _Args) ->
    Msg#msg{from=Msg#msg.to,
            to=Msg#msg.from}.    

rule_filter_test() ->
    NS = process_rules(#pstate{ms=#msg_status{},
                               current=#msg{from="From",
                                            to="To"},
                               msgs=[]},
                       [{filter, ?MODULE, test_filter, []}]),
    ?assert((NS#pstate.current)#msg.from == "To"),
    ?assert((NS#pstate.current)#msg.to == "From").

rules_test() ->
    Rules = [{match, from, "Test", {connection, "Debug-log"}},
             {filter, ?MODULE, test_filter, []},
             {mnp, [no_service_lookups, guess_on_error], from,
              [{telecom, {connection, "Telecom"}},
               {vodafone, {connection, "Vodafone"}}]},
             {connection, "Debug-log 2"}],
    PS = #pstate{ms=#msg_status{}},
    ?assertMatch(#pstate{ms=#msg_status{targets=[{test, {connection, "Debug-log"}}]}},
                 process_rules(PS#pstate{current=#msg{id=test, from="Test"}}, Rules)),
    ?assertMatch(#pstate{ms=#msg_status{targets=[{test, {connection, "Vodafone"}}]}},
                 process_rules(PS#pstate{current=#msg{id=test, to="021767390",from="Test2"}}, Rules)),
    ?assertMatch(#pstate{ms=#msg_status{targets=[{test, {connection, "Vodafone"}}]}},
                 process_rules(PS#pstate{current=#msg{id=test, to="64299950001",from="Test2"}}, Rules)),
    ?assertMatch(#pstate{ms=#msg_status{targets=[{test, {connection, "Debug-log 2"}}]}},
                 process_rules(PS#pstate{current=#msg{id=test, to="3865634564356"}}, Rules)).


