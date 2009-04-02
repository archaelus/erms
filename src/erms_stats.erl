%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Statistics collection server
%% @end
%%%-------------------------------------------------------------------
-module(erms_stats).

-behaviour(gen_server).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").

%% API
-export([start_link/0,
         update_rate/2,
         update_counter/1,
         update_counter/2,
         update_counters/1,
         poll_self/1,
         poll/3,
         current_stats/0,
         clear_stats/0,
         get_counters/1,
         global_get_counters/0,
         global_clear_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {counters,
                rates,
                dump_timer}).
-define(SERVER, ?MODULE).
-define(FIVE_MIN, five_min_interval).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_rate(Rate, Measurement) ->
    gen_server:cast(?SERVER, {update_rate, Rate, Measurement}).

update_counter(Counter) ->
    update_counter(Counter, 1).

update_counter(Counter, Increment) ->
    gen_server:cast(?SERVER, {update_counter, Counter, Increment}).

update_counters(Updates) ->
    gen_server:cast(?SERVER, {update_counters, Updates}).

poll_self(Msg) ->
    poll(self(), Msg, 1000*60*1).

poll(Pid, Msg, Interval) ->
    gen_server:call(?SERVER, {poll, Pid, Msg, Interval}).

current_stats() ->
    gen_server:call(?SERVER, current_stats).

clear_stats() ->
    gen_server:call(?SERVER, clear_stats).

get_counters(Counters) ->
    gen_server:call(?SERVER, {get_counters, Counters}).

global_get_counters() ->
    {GoodNodes, _BadNodes} = erms_rpc:multicall(fun () -> ?MODULE:current_stats() end),
    MergedOD = lists:foldl(fun ({Counters, _Rates}, NodeDict) when is_list(Counters) ->
                                   NodeCtrs = orddict:from_list(Counters),
                                   orddict:merge(fun (_K, A,B) -> A+B end,
                                                 NodeDict,
                                                 NodeCtrs)
                           end,
                           orddict:new(),
                           GoodNodes),
    MergedOD.

global_clear_stats() ->
    erms_rpc:multicall(fun () -> ?MODULE:clear_stats() end).

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
    {ok, TRef} = timer:send_interval(5*60*1000, ?FIVE_MIN),
    {ok, reset_state(#state{dump_timer=TRef})}.

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
handle_call({get_counters, StatKeys}, _From, State = #state{counters=Counters}) ->
    {reply,
     {ok, [case dict:find(K, Counters) of 
               {ok, V} -> {K,V};
               error -> {K, error}
           end
           || K<-StatKeys]},
      State};
handle_call(clear_stats, _From, State) ->
    {reply, ok, reset_state(State)};
handle_call(current_stats, _From, State = #state{counters=Counters,rates=Rates}) ->
    Totals = totals(Counters),
    NewRates = rates(Rates),
    {reply, {Totals, NewRates}, State};
handle_call({poll, Pid, Msg, Interval}, _From, State) ->
    {reply, timer:send_interval(Interval, Pid, Msg), State};
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
handle_cast({update_rate, Rate, Measurement}, State) ->
    {noreply, record_rate(State, Rate, Measurement)};
handle_cast({update_counter, Counter, Increment}, State) ->
    {noreply, bump_counter(State, Counter, Increment)};
handle_cast({update_counters, Updates}, State) ->
    NewCounters = lists:foldl(fun ({Counter, Increment}, Acc) when is_integer(Increment) -> 
                                      bump_counter(Acc, Counter, Increment);
                                  (Counter, Acc) -> 
                                      bump_counter(Acc, Counter, 1)
                              end,
                              State#state.counters,
                              Updates),
    {noreply, State#state{counters=NewCounters}};
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
handle_info(?FIVE_MIN, State = #state{counters=Counters,rates=Rates}) ->
    Totals = totals(Counters),
    NewRates = rates(Rates),
    ?INFO("Writing 5min stats.~nTotals: ~p~nRates: ~p~n", [Totals, NewRates]),
    {noreply, State};
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

record_rate(State = #state{rates=Rates}, Rate, Measurement) ->
    State#state{rates=record_rate(Rates, Rate, Measurement)};
record_rate(Rates, Rate, Measurement) ->
    dict:append(Rate, Measurement, Rates).

bump_counter(State = #state{counters=Counters}, Counter, Increment) ->
    State#state{counters=bump_counter(Counters, Counter, Increment)};
bump_counter(Counters, Counter, Increment) ->
    dict:update_counter(Counter, Increment, Counters).

totals(Counters) ->
    dict:to_list(Counters).

rates(Rates) ->
    dict:fold(fun (Rate, Values, NewRates) ->
                      [{Rate, summarise_values(Values)}|NewRates]
              end,
              [],
              Rates).

summarise_values(Values) ->
    Sum = lists:sum(Values),
    Count = length(Values),
    {lists:min(Values),
     lists:max(Values),
     Sum,
     Count,
     Sum / Count}. 

reset_state(State) ->
    State#state{counters=dict:new(),
                rates=dict:new()}.
