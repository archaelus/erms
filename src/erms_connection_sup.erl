%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(erms_connection_sup).

-include_lib("eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0,children/0,
         child_spec/1,
         start_connection/1, start_connection/2,
         stop_connection/1, stop_connection/2,
         which_connections/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

which_connections() ->
    supervisor:which_children(?SERVER).

start_connection(Term) ->
    start_connection(Term, temporary).

start_connection(C, permanent) when is_record(C, connection) ->
    {atomic, ok} = write_connection(C#connection{supervised=true}),
    start_connection(C, temporary);
start_connection(C, temporary) when is_record(C, connection) ->
    supervisor:start_child(?SERVER,
                           child_spec(C));
start_connection(Name, permanent) ->
    F = fun () -> mnesia:select(connection,
                                ets:fun2ms(fun (#connection{supervised=true,
                                                            name=N,
                                                            _='_'}) when N =:= Name ->
                                                   object()
                                           end))
        end,
    case mnesia:transaction(F) of
        {atomic, [C]} when is_record(C, connection) ->
            start_connection(C, temporary);
        Else ->
            {error, Else}
    end;
start_connection(Name, temporary) ->
    F = fun () -> mnesia:select(connection,
                                ets:fun2ms(fun (#connection{name=N,
                                                            _='_'}) when N =:= Name ->
                                                   object()
                                           end))
        end,
    case mnesia:transaction(F) of
        {atomic, [C]} when is_record(C, connection) ->
            start_connection(C, temporary);
        Else ->
            {error, Else}
    end.

write_connection(C = #connection{}) ->
    mnesia:transaction(fun () ->
                               mnesia:write(C)
                       end).

stop_connection(Name) ->
    stop_connection(Name, temporary).

stop_connection(Name, temporary) ->
    case supervisor:terminate_child(?SERVER, Name) of
        ok ->
            supervisor:delete_child(?SERVER, Name);
        Else -> Else
    end;
stop_connection(Name, permanent) ->
    ok = stop_connection(Name, temporary),
    mnesia:transaction(fun () ->
                               [Con] = mnesia:read(connection, Name, write),
                               mnesia:write(Con#connection{supervised=false})
                       end).



%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok,{{one_for_one,1,10}, children()}}.

%%====================================================================
%% Internal functions
%%====================================================================

children() ->
    F = fun () -> mnesia:select(connection,
                                ets:fun2ms(fun (#connection{supervised=true,_='_', name = Name})
                                               when Name =/= "Debug-Log" ->
                                                   object()
                                           end))
        end,
    {atomic, Connections} = mnesia:transaction(F),
    [child_spec(#connection{name="Debug-Log",
                            mod=erms_debug_connection,
                            args=[],
                            supervised=true})
     | [child_spec(C) || C <- Connections]].

child_spec(C) when is_record(C, connection) ->
    {C#connection.name,
     {C#connection.mod,
      start_link,
      [C#connection.name|C#connection.args]},
     permanent, 2000, worker,
     [C#connection.mod]}.
