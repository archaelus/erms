%%%-------------------------------------------------------------------
%%% File    : erms_sup.erl
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%%% Author  : Geoff Cant <nem@erlang.geek.nz>
%%% Description : ERMS Top level supervisor
%%%
%%% Created :  4 Oct 2006 by Geoff Cant <nem@erlang.geek.nz>
%%%-------------------------------------------------------------------
-module(erms_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
         child_pids/0]).

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
start_link(_) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

child_pids() ->
    [Pid || {_Id, Pid, _Type, _Modules} <- supervisor:which_children(?SERVER)].    

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
    Shortcodes = {'Shortcode Supervisor',
                  {erms_shortcode_sup, start_link, []},
                  permanent, 2000, supervisor,
                  [erms_shortcode_sup]},
    Connections = {'Connection Supervisor',
                   {erms_connection_sup, start_link, []},
                   permanent, 2000, supervisor,
                   [erms_connection_sup]},
    MQ = {'Message Queue',{erms_msg_queue,start_link,[]},
           permanent,2000,worker,[erms_msg_queue]},
    Statistics = {'Stats Collector',{erms_stats,start_link,[]},
                  permanent,2000,worker,[erms_stats]},
    Cache = {'MNP Cache',{erms_mnp_cache,start_link,[]},
             permanent,2000,worker,[erms_mnp_cache,erms_mnp]},
    Archiver = {'Message Archiver',{erms_msg_archiver,start_link,[]},
                  permanent,2000,worker,[erms_msg_archiver]},
    {ok,{{one_for_one,1,10},
         [Statistics,Cache,Archiver,Shortcodes,Connections,MQ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
