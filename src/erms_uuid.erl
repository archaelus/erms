%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ERMS Unique identifiers
%% @end
%%%-------------------------------------------------------------------
-module(erms_uuid).

%% API
-export([next/0, to_list/1,
         nowtime/1,
         utc_datetime/1,
         node/1]).

%%====================================================================
%% API
%%====================================================================
%% @type uuid() = term(). An ERMS universally unique message identifier.
%% @end
%%--------------------------------------------------------------------
%% @spec next() -> uuid()
%% @doc Returns a new unique id.
%% @end 
next() ->
    {erlang:now(), node(), self()}.

%% @spec to_list(UUID::uuid()) -> string()
%% @doc Converts a UUID to a string.
%% @end 
to_list(UUID) ->
    lists:flatten(io_lib:fwrite("~p", [UUID])).

%% @spec nowtime(UUID::uuid()) -> {Megasecs,Secs,Microsecs}
%% @doc Extracts the erlang:now() generation time from a UUID.
%% @end 
nowtime({Now,_Node,_Pid}) ->
    Now.

%% @spec utc_datetime(UUID::uuid()) -> {Date,Time}
%% @doc Extracts the generation time from a UUID in UTC format.
%% @end 
utc_datetime(UUID) ->
    calendar:now_to_universal_time(nowtime(UUID)).

%% @spec node(UUID::uuid()) -> Node::atom()
%% @doc Extracts the node information from a UUID.
%% @end 
node({_Now,Node,_Pid}) ->
    Node.

%%====================================================================
%% Internal functions
%%====================================================================
