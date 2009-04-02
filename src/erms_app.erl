%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%%% File    : erms_app.erl
%%% Author  : Geoff Cant <nem@erlang.geek.nz>
%%% Description : ERMS application module
%%%
%%% Created : 26 Sep 2006 by Geoff Cant <nem@erlang.geek.nz>
%%%-------------------------------------------------------------------
-module(erms_app).

-include_lib("yaws_api.hrl").
-include_lib("yaws.hrl").
-include_lib("logging.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_phase/3]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(normal, _StartArgs) ->
    ok = case application:load(yaws) of
             {error, {already_loaded, yaws}} -> ok;
             ok -> ok
         end,
    ok = application:set_env(yaws, embedded, true),
    ok = case application:start(yaws) of
             {error, {already_started, yaws}} -> ok;
             ok -> ok
         end,
    erms_rb:start(),
    erms_sup:start_link([]).

start_phase(recover, normal, []) ->
    erms_msg_queue:recover(),
    ok;
start_phase(go, normal, []) ->
    {ok, {GL, Sites}} = application:get_env(yaws_config),
    GC = yaws:setup_gconf(GL, yaws_config:make_default_gconf(false, "")),
    yaws_config:add_yaws_soap_srv(GC),
    yaws_api:setconf(GC, []),
    lists:foreach(fun ({_ID, DocRoot, SL}) ->
                          yaws:add_server(DocRoot, SL)
                  end,
                  Sites),
    ok.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
