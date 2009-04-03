%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Caches MNP lookup results
%% @end
%%%-------------------------------------------------------------------
-module(erms_mnp_cache).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0, start/0,
         mnp_lookup/1, cache_result/2, cache_result/3,
         delete_result/1,
         info/0, flush/0, expire/0,
         now_add/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {cache,
                expiry_ref}).
-record(mnp_result, {number, % {Country, Net, Number} format phone number (erms_number:sparse/1)
                     network, % atom indicating number network
                     time % time result was obtained (used for cache expiry)
                    }).
-define(SERVER, ?MODULE).
-define(MAX_AGE_SECS, 60*60*24). % 1 day in seconds - keep mnp results this long
-define(EXPIRY_INTERVAL_MS, timer:hours(1)). % Expire old records every <this> ms.

%%====================================================================
%% API
%%====================================================================
%% @type( mobile_number() :: string() | {string(),string(),string()} ).


%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    case gen_server:start({local, ?SERVER}, ?MODULE, [], []) of
        {error, {already_started, Pid}} -> {ok, Pid};
        OK -> OK
    end.

%% @spec(mnp_lookup/1 :: (mobile_number()) -> 'miss' | {hit, any()}).
mnp_lookup(Number) when is_list(Number) ->
    mnp_lookup(erms_numbers:sparse(Number));
mnp_lookup(Number) when is_tuple(Number) ->
    case ets:lookup(?MODULE, Number) of
        [] -> 
            erms_stats:update_counters([{?MODULE, queries},
                                        {?MODULE, misses}]),
            miss;
        [#mnp_result{network=Network}] ->
            erms_stats:update_counters([{?MODULE, queries},
                                        {?MODULE, hits}]),
            {hit, Network}
    end.


cache_result(Number, Network) ->
    cache_result(Number, Network, erlang:now()).
% Do not cache undefined networks or numbers. Doing so is not helpful.
cache_result(_,undefined,_) ->
    [];
cache_result(undefined,_,_) ->
    [];
cache_result(Number, Network, Time) when is_list(Number) ->
    cache_result(erms_numbers:sparse(Number), Network, Time);
cache_result(Number, Network, Time) when is_tuple(Number) ->
    ets:insert_new(?MODULE, #mnp_result{number=Number,
                                        network=Network,
                                        time=Time}).

info() ->
    gen_server:call(?SERVER, cache_info).

flush() ->
    gen_server:call(?SERVER, flush_cache).

expire() ->
    gen_server:call(?SERVER, expire_cache).

delete_result(Number) when is_list(Number) ->
    delete_result(erms_numbers:sparse(Number));
delete_result(Number) when is_tuple(Number) ->
    ets:delete(?MODULE, Number).
    

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Eref} = timer:apply_interval(?EXPIRY_INTERVAL_MS, ?MODULE, expire, []),
    % Creating the ets table as public for now - this may be
    {ok, #state{cache = ets:new(?MODULE, [named_table, set, public, {keypos, 2}]),
                expiry_ref = Eref}}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(expire_cache, _From, S) ->
    Deleted = ets_expire_cache(?MODULE),
    erms_stats:update_counter({?MODULE, expired}, Deleted),
    {reply, {ok, Deleted}, S};

handle_call(flush_cache, _From, S) ->
    true = ets:delete_all_objects(?MODULE),
    {reply, ok, S};

handle_call(cache_info, _From, S) ->
    {reply, {ok, [{size, ets:info(?MODULE, size)}]}, S};

handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

ets_expire_cache(TableID) ->
    ets:select_delete(TableID,
                      ets_expire_cache_ms(?MAX_AGE_SECS,erlang:now())).

ets_expire_cache_ms(MaxAge, Now) ->
    {MaxAgeMega, MaxAgeSecs, _} = now_add(Now, 0 - MaxAge),
    % Cases
    % Max ages same, check Secs < MaxAgeSecs -> true
    % Mega < MaxAgeMega -> true
    % Mega > MaxAgeMega
    ets:fun2ms(fun (#mnp_result{time={Mega, Secs, _}})
                   when Mega =:= MaxAgeMega, Secs < MaxAgeSecs ->
                       true;
                   (#mnp_result{time={Mega, Secs, _}})
                   when Mega < MaxAgeMega ->
                       true;
                   (_) -> false
               end).

now_add({MegaSecs, Secs, MicroSecs}, OffsetSeconds) ->
    NewSecs = MegaSecs * 1000000 + Secs + OffsetSeconds,
    {NewSecs div 1000000, NewSecs rem 1000000, MicroSecs}.

ms_test() ->
    Now = erlang:now(),
    List = [{#mnp_result{time=now_add(Now, 0 - (2 * ?MAX_AGE_SECS))}, true},
            {#mnp_result{time=now_add(Now, 0 - ?MAX_AGE_SECS - 1)}, true},
            {#mnp_result{time=now_add(Now, 0 - ?MAX_AGE_SECS)}, false},
            {#mnp_result{time=Now}, false},
            {#mnp_result{time=now_add(Now, ?MAX_AGE_SECS)}, false},
            {#mnp_result{time=now_add(Now, ?MAX_AGE_SECS + 1)}, false},
            {#mnp_result{time=now_add(Now, ?MAX_AGE_SECS * 2)}, false}],
    CompMS = ets:match_spec_compile(ets_expire_cache_ms(?MAX_AGE_SECS, Now)),
    ExpectedMatchResults = [Match || {_,Match} <- List],
    ?assertMatch(ExpectedMatchResults,
                 ets:match_spec_run([Result || {Result,_} <- List], CompMS)).

ct_submit_to_cache(List) ->
    [cache_result(Number,Network) || {Number,Network} <- List].

ct_check_exists([]) ->
    [true, ""];
ct_check_exists([{Number,Network}|Rest]) ->
    case mnp_lookup(Number) of
        {hit, Network} ->
            ct_check_exists(Rest);
        miss ->
            [false,Number,erms_numbers:sparse(Number)]
    end.

% Check to see if we don't find the number/network items in the cache.
% We may find the number, but it should not be on the specified network.
ct_check_misses([]) ->
    [true, ""];
ct_check_misses([{Number, Network}|Rest]) ->
    case mnp_lookup(Number) of
        {hit, Network} ->
            [false,{not_missed,Number}];
        {hit,_}->
            % We found it, but it had a different network. That's fine.
            ct_check_misses(Rest);
        miss ->
            ct_check_misses(Rest)
    end.


cache_test() ->
    start(),
    % List of phone numbers to cache.
    CacheThese = [{"0213456789",vodafone}, % Random
    {"64212740452",vodafone}, % Vik's Vodafone
    {"027 270 2438",telecom}, % Sanyo 4290, postpaid, telecom
    {"027 359 1743",telecom}, %Sanyo 7500, prepay, telecom
    {"021 2954013",telecom} % Sanyo 8400, prepay, ported number, telecom
    ],

    % List of numbers we expect to find (we've changed the format but not value).
    FindThese = [{"64213456789",vodafone},{"0212740452",vodafone},
        {"027 270 2438",telecom},{"027 3591743",telecom}, {"021 2954013",telecom}],
    % Numbers we DO NOT expect to find, so we don't just say "yes" all the time.
    MissThese  = [{"61213456789",vodafone},{"021274045",vodafone}, {"021 2954013",vodafone},{"64212740452",telecom}],
    ct_submit_to_cache(CacheThese),
    FoundResults=ct_check_exists(FindThese),
    ?assertMatch([true,_],FoundResults),
    MissedResults=ct_check_misses(MissThese),
    ?assertMatch([true,_],MissedResults).
