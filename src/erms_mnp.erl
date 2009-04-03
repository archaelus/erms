%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc This module provides Mobile Number Portability lookup functions.

-module(erms_mnp).
-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(STAGING_LOOKUP_URL,
        "http://mnps.wapflash.net.nz:8080/mnp/query?service=isTelecomCustomer&MDN=").

%% API
-export([mnp_lookup/2, mnp_service_lookup/2, app_lookup_url/0, lookup_url/1,
         valid_options/1]).

%%====================================================================
%% api
%%====================================================================

%% @doc Performs a MNP service lookup to determine whether a given
%% Number belongs to the vodafone or telecom networks using a default
%% MNP service URL. 
%% @see mnp_lookup/2
%% @end
%% @spec (Number, Options::list()) -> Network::atom() | {error, Reason::term()}
%%   Number = {Country::string(), Net::string(), Sub::string()} | string()
%% @end

% We only operate on parsed numbers, not ph# strings
mnp_lookup(Number, Options) when is_list(Number) ->
    mnp_lookup(erms_numbers:sparse(Number), Options);
mnp_lookup(Number, Options) when is_tuple(Number) ->
    check_parse_error(Number, expand(Options)).

% Standard option sets for mnp lookup (given there are a zillion of them now)
expand(Options) ->
    proplists:normalize(Options,
                        [{expand, [{standard_meta_mnp,
                                    [assume_vodafone, no_ipms, guess_on_error,
                                     atom_response, use_cache]},
                                   {uncached_meta_mnp,
                                    [assume_vodafone, no_ipms, guess_on_error,
                                     atom_response]},
                                   {no_service_lookups,
                                    [no_ipms, no_meta]}]}
                        ]).

valid_options(Options) ->
    erms_config:vand(lists:map(fun valid_option/1, expand(Options))).

valid_option(assume_vodafone) -> valid;
valid_option(no_ipms) -> valid;
valid_option(guess_on_error) -> valid;
valid_option(no_meta) -> valid;
valid_option(atom_response) -> valid;
valid_option(use_cache) -> valid;
valid_option(BadOption) -> {error, {"Bad mnp option", BadOption}}.

% {error, Reason} is the erms_numbers:parse error case.
check_parse_error({error, Reason}, Options) ->
    Invalid = proplists:get_bool(error_invalid, Options),
    if Invalid -> invalid;
       true -> {error, Reason}
    end;
check_parse_error(Number, Options) ->
    check_non_mnp_networks(Number, Options).

% Old telecom network
check_non_mnp_networks({_Country, "25", _Sub}, Options) ->
    Valid = proplists:get_bool(valid_025, Options),
    if Valid -> telecom;
       true -> invalid
    end;
% Telecom pager network
check_non_mnp_networks({_Country, "26", _Sub}, Options) ->
    Valid = proplists:get_bool(valid_026, Options),
    if Valid -> telecom;
       true -> invalid
    end;
check_non_mnp_networks(Number, Options) ->
    check_cache(Number, Options).

check_cache(Number, Options) ->
    UseCache = proplists:get_bool(use_cache, Options),
    cache_lookup(UseCache, Number, Options).

cache_lookup(true, Number, Options) ->
    case erms_mnp_cache:mnp_lookup(Number) of
        {hit, Result} ->
            Result;
        miss -> 
            Result = service_lookups(Number, Options),
            erms_mnp_cache:cache_result(Number, Result),
            Result
    end;
cache_lookup(false, Number, Options) ->
    service_lookups(Number, Options).

service_lookups(Number, Options) ->
    NoIpms = proplists:get_bool(no_ipms, Options),
    NoMeta = proplists:get_bool(no_meta, Options),
    Result = if NoIpms and NoMeta -> erms_numbers:guess_network(Number);
                NoIpms -> meta_lookup(Number, Options);
                true -> ipms_lookup(Number, Options)
             end,
    interpret_result(Result, Number, Options).

ipms_lookup(Number, _Options) ->
    case erms_ipms_cache:lookup(Number) of
        not_ported -> erms_numbers:guess_network(Number);
        E -> E
    end.

meta_lookup(Number, _Options) ->
    case mnp_service_lookup(erms_numbers:format(mdn, Number), app_lookup_url()) of
        {error, timeout} ->
            erms_stats:update_counter({?MODULE, mnp_lookup, timeout}),
            {error, timeout};
        Else -> 
            erms_stats:update_counter({?MODULE, mnp_lookup, success}),
            Else
    end.

interpret_result(Result, Number, Options) ->
    GuessOnError = proplists:get_bool(guess_on_error, Options),
    ErrorInvalid = proplists:get_bool(error_invalid, Options),
    GuessOnUnknown = proplists:get_bool(guess_on_unknown, Options),
    AssumeVodafone = proplists:get_bool(assume_vodafone, Options),
    AtomResponse = proplists:get_bool(atom_response, Options),
    case Result of
        not_telecom when AssumeVodafone -> vodafone;
        unknown when GuessOnUnknown -> erms_numbers:guess_network(Number);
        {error, _} when GuessOnError -> erms_numbers:guess_network(Number);
        {error, _} when ErrorInvalid -> invalid;
        {invalid, _Reason} when AtomResponse -> invalid;
        Atom when is_atom(Atom) -> Atom;
        Error -> Error
    end.

%% @doc Performs a MNP service lookup to determine whether a
%% given Number belongs to the vodafone or telecom networks. The Number
%% argument is appended to the Url argument to create the MNP service
%% lookup query URL.
%% @end
%% @spec (string(), string()) -> telecom | not_telecom | {error, Reason}
mnp_service_lookup(Number, Url) ->
    case http:request(get, {lookup_url(Number, Url), []}, [{timeout, 5000}], []) of
        {ok, Resp} ->
            parseResponse(Resp);
        Error ->
            {error, {mnp_lookup, Error}}
    end.

mnp_lookup_test() ->
    ?assertMatch(telecom, mnp_lookup("+64 26 123456", [guess_on_error,valid_026])),
    ?assertMatch(vodafone, mnp_lookup("+64 2885000389", [guess_on_error,no_service_lookups])),
    ?assertMatch(telecom, mnp_lookup("+64 25 123456", [guess_on_error,valid_025])),
    ?assertMatch({invalid, _}, mnp_lookup("0217", [guess_on_error,no_service_lookups])).

mnp_lookup_connected_test() ->
    ?assertMatch(vodafone, mnp_lookup("021767390", [uncached_meta_mnp])),
    ?assertMatch(telecom, mnp_lookup(erms_numbers:format(mdn, "+64 29 995-0001"), [uncached_meta_mnp])),
    ?assertMatch(telecom, mnp_lookup("+64 29 995-0001", [uncached_meta_mnp])),
%    ?assert(mnp_lookup("021 221 8021", [uncached_meta_mnp]) == telecom),
    ?assertMatch(vodafone, mnp_lookup("021 221 0000", [uncached_meta_mnp])),
    ?assertMatch(invalid, mnp_lookup("0217", [uncached_meta_mnp])).

invalid_nets_test() ->
    ?assertMatch(invalid, mnp_lookup("025957150", [invalid_025])),
    ?assertMatch(vodafone, mnp_lookup("021767390", [invalid_025,
                                                    uncached_meta_mnp])),
    ?assertMatch(invalid, mnp_lookup("026 255 7433", [invalid_026])).


standard_meta_mnp_test() ->
    {ok, _} = erms_mnp_cache:start(),
    ?assertMatch(miss,
                 erms_mnp_cache:mnp_lookup(erms_numbers:sparse("+64 21 767 390"))),
    ?assertMatch(vodafone, mnp_lookup("+64 21 767 390", [uncached_meta_mnp])),
    ?assertMatch(miss,
                 erms_mnp_cache:mnp_lookup(erms_numbers:sparse("+64 21 767 390"))),
    ?assertMatch(vodafone, mnp_lookup("+64 21 767 390", [standard_meta_mnp])),
    ?assertMatch({hit, vodafone},
                 erms_mnp_cache:mnp_lookup(erms_numbers:sparse("+64 21 767 390"))).
    

%%====================================================================
%% Internal functions
%%====================================================================
lookup_url(Number) ->
    lookup_url(Number, app_lookup_url()).

lookup_url(Number, Url) ->
    Url ++ Number.

parseResponse({{_Ver, Code, _Resp},
               _Headers,
               Text})
 when 200 =< Code, Code =< 299 ->
    case Text of
        "1\r\n" ->
            telecom;
        "0\r\n" ->
            not_telecom;
        "1\n" ->
            telecom;
        "0\n" ->
            not_telecom;
        Other ->
            {invalid, Other}
    end;
parseResponse(Error) ->
    ?WARN("Meta lookup failed: ~p", [Error]),
    {error, Error}.

app_lookup_url() ->
    case application:get_env(erms, mnp_service_url) of
        {ok, Url} ->
            Url;
        undefined ->
            ?WARN("Missing application env var: mnp_service_url.", []),
            ?STAGING_LOOKUP_URL
    end.
