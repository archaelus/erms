%%%-------------------------------------------------------------------
%%%@copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Connection registration and location manager
%% @end
%%%-------------------------------------------------------------------
-module(erms_connection_mgr).

%% API
-export([reg/1,reg/2,unreg/1,where/1]).

%%====================================================================
%% API
%%====================================================================

reg(Name) ->
    proc_reg:reg(Name, self()).

reg(Name, Pid) ->
    proc_reg:reg(Name, Pid).

unreg(Name) ->
    proc_reg:unreg(Name).

where(Name) ->
    proc_reg:where(Name).

%%====================================================================
%% Internal functions
%%====================================================================
