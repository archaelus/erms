%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(erms_shortcode_sup).

-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, child_spec/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    {atomic, Specs} = mnesia:transaction(fun shortcode_specs/0),
    {ok, {{one_for_one,1,10},Specs}}.

%%====================================================================
%% Internal functions
%%====================================================================

shortcode_specs() ->
    S = mnesia:select(shortcode,
                      ets:fun2ms(fun (#shortcode{name=Name}) ->
                                         Name
                                 end)),
    lists:map(fun child_spec/1, S).

child_spec(#shortcode{name=Name}) ->
    child_spec(Name);
child_spec(Name) ->
    {Name,
     {erms_shortcode,start_link,[Name]},
     permanent, 2000, worker,
     [erms_shortcode, erms_shortcode_lib]}.
