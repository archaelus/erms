%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Process that makes delivery runs over the targets
%%  in #msg_status.targets until all delivery attempts
%%  succeed or the message expires.
%% @end
-module(erms_msg_queue_processor).

-include_lib("logging.hrl").
-include_lib("eunit.hrl").
-include_lib("mnesia_model.hrl").

-export([start_link/1, init/2,
         record_success/3, record_failure/4,
         schedule_retry/1]).

-define(DELIVER_WAIT, infinity).
-define(RETRY_BASE_INTERVAL, 60 * 1000). % 1 minute timer:minutes(1)
-define(RETRY_FUZZ, 60 * 1000). % retries within 1 min of target interval
-define(RETRY_MAX_INTERVAL, 2 * 60 * 60 * 1000). % 2 hours (timer:hours(2))
-define(DELIVER_ROUTE_TIMEOUT, 60 * 1000). % allow one minute for router to respond

-define(SECS_IN_DAY, 60 * 60 * 24). %seconds in a day

start_link(S) when is_record(S, msg_status) ->
    proc_lib:start_link(?MODULE, init, [self(), S]).

% ---------------------------------------------------------------------------------

% Expired/delivered message - ignore
init(Parent, #msg_status{state = S}) when S == delivered; S == expired ->
    proc_lib:init_ack(Parent, {ignore, self()}),
    ok;
init(Parent, Status) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    check_new(Status).

% If message is new, route it
check_new(#msg_status{state = new} = S) ->
    {ok, RouteFn} = erms_shortcode:route(S, ?DELIVER_ROUTE_TIMEOUT),
    try RouteFn() of
        Status = #msg_status{state = in_progress} ->
            process(Status);
        Else ->
            ?ERR("Couldn't route message (~p), ~p", [S#msg_status.msg_id, Else])
    catch
        Term ->
            ?ERR("Couldn't route message (~p), ~p", [S#msg_status.msg_id, Term]),
            expire(S)
    end;
% Otherwise, process as normal.
check_new(S) ->
    process(S).

% Check expiry and message start time
process(S) ->
    process(S,
            message_expired(S),
            message_deliverable(S)).
% Message has expired.
process(S, true, _) ->
    expire(S);
% Message has not become deliverable yet
process(S, false, false) ->
    wait(S),
    process(S);
% Deliver the message.
process(S, false, true) ->
    deliver(S).

expire(Status) ->
    ?INFO("Message ~p expired before delivery.", [Status#msg_status.msg_id]),
    NewStatus = Status#msg_status{state=expired},
    {atomic, ok} = mnesia:transaction(fun () -> mnesia:write(NewStatus) end),
    erms_stats:update_counter(message_expired),
    ok.

deliver(Status) ->
    LiveTargets = live(Status),
    deliver(Status, LiveTargets, [], 1).

% Sleeps until the specified delivery_time.
wait(#msg_status{delivery_time=undefined}) ->
    ok;
wait(#msg_status{delivery_time=StartTime}) ->
    SleepTimeMilliSecs = datetime_diff(erlang:universaltime(), StartTime) * 1000,
    if SleepTimeMilliSecs > 0 -> 
            ?INFO("Sleeping ~p ms until message is deliverable.", [SleepTimeMilliSecs]),
            timer:sleep(SleepTimeMilliSecs);
       true -> ok
    end.

% Delivery is finished:
deliver(Status, [], [], _Tries) ->
    NewStatus = Status#msg_status{state=delivered},
    {atomic, ok} = mnesia:transaction(fun () -> mnesia:write(NewStatus) end),
    erms_stats:update_counter(delivery_complete),
    ok;
% Delivery run complete
deliver(Status, [], ToRetry, Tries) ->
    schedule_retries(Status, ToRetry, Tries);
% Delivery attempt on a delivery run.
deliver(Status, [{MsgId, Dest}|Rest], ToRetry, Tries) ->
    Msg = erms_msg:get_msg(MsgId),
    case erms_connection:sync_deliver(Dest, Msg, infinity) of
        ok ->
            record_success(Status#msg_status.msg_id, MsgId, Dest),
            deliver(Status, Rest, ToRetry, Tries);
        Err ->
            record_failure(Status#msg_status.msg_id, MsgId, Dest, Err),
            deliver(Status, Rest, [{MsgId, Dest}|ToRetry], Tries)
    end.

% Record a delivery target success
record_success(OrigMsgId, MsgId, Dest) ->
    erms_stats:update_counter(delivery_success),
    E = #delivery_success{orig_msg_id=OrigMsgId,
                          msg_id=MsgId,
                          destination=Dest,
                          time=erlang:now()},
    {atomic, ok} = mnesia:transaction(fun () ->
                                              mnesia:write(E)
                                      end).
% Record a delivery target failure
record_failure(OrigMsgId, MsgId, Dest, Err) ->
    ?WARN("Couldn't deliver message ~p to ~p: ~p", [MsgId, Dest, Err]),
    erms_stats:update_counter(delivery_failure),
    E = #delivery_failure{orig_msg_id=OrigMsgId,
                          msg_id=MsgId,
                          destination=Dest,
                          reason=Err,
                          time=erlang:now()},
    {atomic, ok} = mnesia:transaction(fun () ->
                                              mnesia:write(E)
                                      end).

schedule_retries(Status, Targets, Tries) ->
    schedule_retries(Status, Targets, Tries, message_expired(Status)).

% Message expired
schedule_retries(Status, _Targets, _Tries, true) ->
    expire(Status),
    ok;
% Message retry
schedule_retries(Status, Targets, Tries, false) ->
    erms_stats:update_counter(delivery_retry),
    SleepTime = schedule_retry(Tries),
    ?INFO("Retrying message delivery in ~p ms.", [SleepTime]),
    timer:sleep(SleepTime),
    deliver(Status, Targets, [], Tries + 1).

%% @spec retry_interval(Tries::integer()) -> integer()
%% @doc Returns the interval in milliseconds until the next try
%%  based on the number of tries attempted so far. Fuzzed, capped
%%  exponential backoff.
%% @end
retry_interval(Tries) ->
    Target = round(?RETRY_BASE_INTERVAL * math:pow(2, (Tries - 1))),
    Fuzz = random:uniform(?RETRY_FUZZ),
    Target - Fuzz.

schedule_retry(Tries) ->
    Interval = retry_interval(Tries),
    if (Interval > ?RETRY_MAX_INTERVAL) -> ?RETRY_MAX_INTERVAL;
       true -> Interval
    end.

%% @spec message_expired(#msg_status{}) -> bool()
%% @doc True if a message was queued more than ?RETRY_MAX_INTERVAL ago.
%% @end
message_expired(#msg_status{msg_id=Id, expiry_time=undefined}) ->
    MsgSubmissionTime = erms_uuid:utc_datetime(Id),
    expired(MsgSubmissionTime, datetime_add(MsgSubmissionTime, ?RETRY_MAX_INTERVAL));
message_expired(#msg_status{msg_id=Id, expiry_time=Expiry}) ->
    MsgSubmissionTime = erms_uuid:utc_datetime(Id),
    expired(MsgSubmissionTime, Expiry).

%% @spec message_deliverable(#msg_status{}) -> bool()
%% @doc True if the message delivery_time is in the past.
%% @end
message_deliverable(#msg_status{delivery_time=undefined}) ->
    true;
message_deliverable(#msg_status{delivery_time=T}) ->
    datetime_diff(T, erlang:universaltime()) > 0.

%% @spec expired(T1::Time, T2::Time) -> bool()
%%  Time = {{Y,M,D},{H,M,S}} | integer()
%% @doc Returns true if T1 > T2. (T2 is the expiry time, T1 is now)
%% @end
expired(T1, T2) when is_tuple(T1) ->
    T1S = calendar:datetime_to_gregorian_seconds(T1),
    expired(T1S, T2);
expired(T1S, T2) when is_integer(T1S), is_tuple(T2) ->
    T2S = calendar:datetime_to_gregorian_seconds(T2),
    expired(T1S, T2S);
expired(T1S, T2S) when is_integer(T1S), is_integer(T2S) ->
    T1S > T2S.

datetime_add(T, Interval) ->
    S = calendar:datetime_to_gregorian_seconds(T),
    calendar:gregorian_seconds_to_datetime(S + Interval).

datetime_diff(T1, T2) ->
    calendar:datetime_to_gregorian_seconds(T2) -
        calendar:datetime_to_gregorian_seconds(T1).

%% @doc Returns the {MsgId, Dest} targets that haven't been successfully delivered.
%% @end
live(#msg_status{msg_id = OrigMsgId, targets = Targets} = _Status) ->
    {atomic, Successes} = 
        mnesia:transaction(fun () ->
                                   mnesia:read(delivery_success,
                                               OrigMsgId,
                                               read)
                           end),
    lists:filter(fun ({TargetMsgId, Dest}) ->
                         live(OrigMsgId, TargetMsgId, Dest, Successes)
                 end,
                 Targets).

live(_,_,_,[]) ->
    true;
live(OrigMsgId, MsgId, Dest, 
     [#delivery_success{orig_msg_id = OrigMsgId,
                        msg_id = MsgId,
                        destination = Dest} = _Success
      |_]) ->
    false;
live(OrigMsgId, MsgId, Dest, [_|Rest]) ->
    live(OrigMsgId, MsgId, Dest, Rest).

live1_test() ->
    ?assert(live(orig_id, id, dest, [#delivery_success{orig_msg_id = orig_id,
                                                       msg_id = id,
                                                       destination = dest}]) == false).
live2_test() ->
    ?assert(live(orig_id, id, dest, [#delivery_success{orig_msg_id = orig_id,
                                                       msg_id = wrong_id,
                                                       destination = dest}]) == true).
live3_test() ->
    ?assert(live(orig_id, id, dest, [#delivery_success{orig_msg_id = orig_id,
                                                       msg_id = id,
                                                       destination = wrong_dest}]) == true).
live4_test() ->
    ?assert(live(orig_id, id, dest, [#delivery_success{orig_msg_id = orig_id,
                                                       msg_id = id,
                                                       destination = wrong_dest},
                                     #delivery_success{orig_msg_id = orig_id,
                                                       msg_id = id,
                                                       destination = dest}
                                    ]) == false).

schedule_retry_test() ->
    ?assert(lists:all(fun (N) when 
                          N >= 0,
                          N =< ?RETRY_MAX_INTERVAL ->
                              true;
                          (_) -> false
                      end,
                      lists:map(fun schedule_retry/1,
                                lists:seq(1, 50)))).

expiry_test() ->
    Now = erlang:universaltime(),
    Past = datetime_add(Now, -86400),
    Future = datetime_add(Now, 86400),
    Id = erms_uuid:next(),
    ?assert(message_expired(#msg_status{msg_id=Id, expiry_time=Future}) == false),
    ?assert(message_expired(#msg_status{msg_id=Id, expiry_time=Past}) == true).

expiry_standalone_test() ->
    Now = erlang:universaltime(),
    Past = datetime_add(Now, -86400),
    Future = datetime_add(Now, 86400),
    ?assert(expired(Now, Future) == false),
    ?assert(expired(Now, Past) == true).
