%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc RPC utilities and wrappers.
%% @end
%%%-------------------------------------------------------------------
-module(erms_rpc).

-include_lib("eunit.hrl").

%% API
-export([erms_nodes/0,
         multicall/1,
         multicall/2,
         call_with_other_nodes/1,
         call_with_other_nodes/2,
         othernodes/1,
         othernodes/2]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec erms_nodes() -> [Node]
%%   Node = atom()
%% @doc Returns a list of all connected nodes where erms is currently
%%      running. (currently searches for a running erms_stats process)
%% @end 
erms_nodes() ->
    {Good, _} = rpc:multicall(erlang, whereis, [erms_stats]),
    [Node || Node <- Good,
             Node =/= undefined ].

%% @spec multicall(Fun::function()) -> {GoodNodes::list(),BadNodes::list()}
%% @doc Calls erlang:apply(Fun, []) on all erms_nodes().
%% @end
multicall(Fun) when is_function(Fun,0) ->
    multicall(erms_nodes(), Fun).

%% @spec multicall(Nodes, Fun::function()) -> {GoodNodes::list(),BadNodes::list()}
%%   Nodes = [atom()]
%% @doc Calls erlang:apply(Fun, []) on all Nodes.
%% @end
multicall(Nodes, Fun) when is_list(Nodes), is_function(Fun,0) ->
    rpc:multicall(Nodes, erlang, apply, [Fun, []]).

%% @spec call_with_other_nodes(Fun::function) -> Result
%%   Result = [{Node, {GoodNodes::list(), BadNodes::list()}}]
%%   Node = atom()
%% @doc Calls erlang:apply(Fun, [OtherErmsNodes::list()]) on all 
%%      erms_nodes().
%% @end
call_with_other_nodes(Fun) when is_function(Fun) ->
    call_with_other_nodes(erms_nodes(), Fun).

%% @spec call_with_other_nodes(Nodes, Fun::function) -> Result
%%   Result = [{Node, {GoodNodes::list(), BadNodes::list()}}]
%%   Node = atom()
%%   Nodes = [Node]
%% @doc Calls erlang:apply(Fun, [OtherErmsNodes::list()]) on all 
%%      Nodes.
%% @end
call_with_other_nodes(Nodes, Fun) when is_function(Fun) ->
    lists:map(fun (Node) ->
                      {Node, rpc:multicall(Node, erlang, apply, [Fun, othernodes(Node, Nodes)])}
              end,
              Nodes).

%% @spec othernodes(Nodes) -> Nodes
%%   Nodes = [Node::atom()]
%% @doc Removes the current node() from a list of Nodes.
%% @end
othernodes(Nodes) ->
    othernodes(node(), Nodes).

%% @spec othernodes(Node,Nodes) -> Nodes
%%   Node = atom()
%%   Nodes = [Node]
%% @doc Removes the Node from a list of Nodes.
%% @end
othernodes(Node, Nodes) ->
    lists:filter(fun (N) -> N =/= Node end, Nodes).
    
