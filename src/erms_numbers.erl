%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc This library provides mobile number parsing and formatting
%% functions.
%% @end
%%%-------------------------------------------------------------------
-module(erms_numbers).
-include_lib("eunit/include/eunit.hrl").
-include_lib("mnesia_model.hrl").

-import(gregexp, [groups/2]).
-import(lists, [reverse/1, split/2]).
-import(erms_msg, [msg_field/2,msg_field/3,clone/1]).

%% API
-export([strip_non_digits/1, parse/1, format/2, guess_network/1, sparse/1,
         msg_filter/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Removes non [0-9] chars from a string.
%% @spec (Number::string()) -> string()
strip_non_digits(Number) ->
    reverse(strip_non_digits(Number, [])).

strip_non_digits([D|Rest], SoFar) when $0 =< D, D =< $9 ->
    strip_non_digits(Rest, [D|SoFar]);
strip_non_digits([_D|Rest], SoFar) ->
    strip_non_digits(Rest, SoFar);
strip_non_digits([], SoFar) ->
    SoFar.

strip_non_digits_test() ->
    ?assert(strip_non_digits("(+64) 21 767-390") == "6421767390").

%% @doc Splits a phone number into it's Country, Network and Subscriber parts.
%% @spec (Number) -> {Country, Network, Subscriber} | {error, Reason}
%%   Number = string() | {Country, Network, Subscriber}
%%   Country = string()
%%   Network = string()
%%   Subscriber = string()
parse({Country,Network,Subscriber}) ->
    {Country,Network,Subscriber};
parse(Number) when is_list(Number) ->
    RE = "^\\(64|61|0\\)?\\(2[0-9]\\)\\([0-9]+\\)$",
    case groups(Number, RE) of
        {match, [Country, Network, Subscriber]} ->
            {normalise_country(Country), Network, Subscriber};
        {match, [Network, Subscriber]} ->
            {"64", Network, Subscriber};
        Err ->
            {error, Err}
    end;
parse(Something) ->
    {error, {parse, Something}}.

parse_test() -> 
    ?assert(parse("6421767390") == {"64", "21", "767390"}),
    ?assert(parse("021767390") == {"64", "21", "767390"}),
    ?assert(parse("026123456") == {"64", "26", "123456"}),
    ?assert(parse(parse("6421767390")) == parse("6421767390")),
    ?assertMatch({error, _}, parse(fiddlesticks)).

%% @doc Removes non-digit chars before parsing number.
%% @see parse/1
sparse({Country,Network,Subscriber}) ->
    {normalise_country(Country),Network,Subscriber};
sparse(Number) ->
    parse(strip_non_digits(Number)).

%% @doc Guesses the network operator for a network number.
%% Not MNP compliant, but a good fallback.
%% @spec (NetworkInfo) -> vodafone | telecom | unknown
%%   NetworkInfo = Network::string() | {term(), Network::string(), term()}
guess_network({_Country, Net, Sub}) when is_list(Sub),
                                         6 =< length(Sub),
                                         length(Sub) =< 8 ->
    guess_network(Net);
guess_network({_Country, _Net, _Sub}) ->
    {invalid, "Subscriber number is not 6-8 chars."};
guess_network(Net)
  when Net == "21"; Net == "29" ->
    vodafone; % telstraclear is vodafone
guess_network(Net) 
  when Net == "25"; Net == "27"; Net == "26" ->
    telecom;
guess_network("28") ->
    vodafone;   %call_plus or M2;
guess_network("20") ->
    orcon;
guess_network("24") ->
    vodafone; %ihug
guess_network(_Other) ->
    unknown.

guess_network_test() ->
    ?assert(guess_network("21") == vodafone),
    ?assert(guess_network("22") == unknown),
    ?assert(guess_network("23") == unknown),
    ?assert(guess_network("24") == vodafone),
    ?assert(guess_network("25") == telecom),
    ?assert(guess_network("26") == telecom),
    ?assert(guess_network("27") == telecom),
    ?assert(guess_network("28") == vodafone),
    ?assert(guess_network("29") == vodafone),
    ?assert(guess_network("2") == unknown).

normalise_country("0") ->
    "64";
normalise_country(Other) ->
    Other.

normalise_country_test() ->
    ?assert(normalise_country("0") == "64"),
    ?assert(normalise_country("64") == "64"),
    ?assert(normalise_country("61") == "61").

%% @doc Formats a parsed number in the specified format.
%% Number will be sparsed if it's not a 3-tuple.
%% Telecom/MDN/e123 format will fail if the country code is not NZ (64 or 0).
%% @see sparse/1
%% @spec (Format, Number) -> string() | {error, Reason}
%%  Format = e164 | e164_pretty | e123 | e123_pretty | vodafone | telecom | mdn
%%  Number = string() |
%%           {Country::string(), Network::string(), Subscriber::string()}
format(e164, {Country, Network, Subscriber}) ->
    normalise_country(Country) ++ Network ++ Subscriber;
format(e164_pretty, {Country, Network, Subscriber}) ->
    "(+" ++ normalise_country(Country) ++ ") " ++ Network ++ " " ++ pretty_subscriber(Subscriber);
format(e123, {Country, Network, Subscriber}) ->
    case normalise_country(Country) of
        "64" ->
            "0" ++ Network ++ Subscriber;
        NotNZ ->
            throw({bad_country, NotNZ})
    end;
format(e123_pretty, {Country, Network, Subscriber}) ->
    case normalise_country(Country) of
        "64" ->
            "0" ++ Network ++ " " ++ pretty_subscriber(Subscriber);
        NotNZ ->
            throw({bad_country, NotNZ})
    end;        
format(vodafone, Number) -> % vodafone/e164 -- synonyms
    format(e164, Number);
format(telecom, Number) ->
    format(e123, Number);
format(mdn, Number) -> % telecom/mdn/e123 -- synonyms
    format(e123, Number);
format(Format, Number) when is_list(Number) ->
    format(Format, sparse(Number)).

format_test() ->
    ?assert(format(vodafone, "(+64) 21 767-390") == "6421767390"),
    ?assert(format(telecom, "(+64) 21 767-390") == "021767390"),
    ?assert(format(mdn, "+64 27 241 7373") == "0272417373").

pretty_subscriber(Number) ->
    pretty_subscriber(Number, " ").
pretty_subscriber(Number, Separator) ->
    Half = length(Number) div 2,
    {S1, S2} = split(Half, Number),
    S1 ++ Separator ++ S2.

pretty_subscriber_test() ->
    ?assert(pretty_subscriber("123456") == "123 456"),
    ?assert(pretty_subscriber("1234567") == "123 4567"),
    ?assert(pretty_subscriber("12345678") == "1234 5678").

combined_test() ->
    ?assert(sparse("(+64) 21 767-390") == {"64", "21", "767390"}),
    ?assert(guess_network(sparse("(+64) 21 767-390")) == vodafone).

msg_filter(Msg, [Format|Fields]) ->
    lists:foldl(fun (Field,M) ->
                        msg_field(M, Field,
                                  format(Format, msg_field(M, Field)))
                end,
                clone(Msg),
                Fields).

msg_filter_test() ->
    ?assertMatch(#msg{from="6421767390",to="021767390"},
                 msg_filter(#msg{from="021767390",to="021767390"},
                            [e164,from])).
