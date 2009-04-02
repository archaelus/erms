%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(healthcheck_simulator).
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").

-export([start_link/2, start_link/3, init/1, loop/1]).

start_link(Host, Port) ->
    start_link(Host, Port, timer:seconds(5)).
start_link(Host, Port, CheckMS) ->
    proc_lib:spawn_link(healthcheck_simulator, init, [{self(), Host, Port, CheckMS}]).

init({Parent, Host, Port, CheckMS}) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    loop({Host, Port, CheckMS});
init(T) ->
    ?ERR("Fail. ~p", [T]),
    boh.

loop({Host, Port, CheckMS} = State) ->
    Start = erlang:now(),
    case gen_tcp:connect(Host, Port, [{active, false}], CheckMS) of
        {ok, Sock} ->
            %?INFO("Healthcheck succeeded in ~p ms.", [elapsed(Start)]),
            gen_tcp:close(Sock);
        {error, Reason} ->
            ?INFO("Healthcheck failed in ~p ms. (~p)", [elapsed(Start), Reason])
    end,
    schedule(Start, CheckMS),
    ?MODULE:loop(State).
            
    
elapsed(Start) ->
    timer:now_diff(erlang:now(), Start) div 1000.

schedule(Start, Interval) ->
    case elapsed(Start) of
        Time when Time >= Interval ->
            ok;
        Time ->
            timer:sleep(Interval - Time)
    end.
