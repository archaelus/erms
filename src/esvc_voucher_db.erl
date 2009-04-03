%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Vik Olliver <vik@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Voucher database management code
%% @end
%%%-------------------------------------------------------------------
-module(esvc_voucher_db).
-include_lib("eunit/include/eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("esvc_voucher.hrl").

% @todo
% list_campaigns
% delete_campaign

%%====================================================================
%% API
%%====================================================================
%% foo
%% @end
-export([import_campaign_file/1,
         list_campaigns/0,
         list_campaign_counters/1]).

%%--------------------------------------------------------------------
%% @spec import_campaign_file(FileName::string()) -> term()
%% @doc Reads a CSV Campaign file into an Mnesia database ready for use
%% by voucher code. If used by humans, please run mnesia:start() first.
%% @end 
import_campaign_file(FName) ->
    import_campaigns(esvc_voucher_import:read_campaign_file(FName)).

%% @spec import_campaigns([#campaign{}]) -> term()
import_campaigns(Campaigns) when is_list(Campaigns) ->
    mnesia:transaction(fun () ->
                               foreach_eq(fun mnesia:write/1, ok, Campaigns)
                       end).


%%--------------------------------------------------------------------
%% @spec list_campaigns() -> [ListOfCampaigns]
%% @doc Reads an Mnesia database of campaigns into a list ready for use
%% by voucher code. If used by humans, please run mnesia:start() first.
%% @end 
list_campaigns() ->
    F = fun () -> mnesia:select(campaign, ets:fun2ms(fun (#campaign{}) -> object() end)) end,
    {atomic, Campaigns} = mnesia:transaction(F),
    Campaigns.

%%--------------------------------------------------------------------
%% @spec list_campaign_counters(Date) -> [ListOfCounters]
%% @doc Lists all the counters used for campaign statistics for the
%% specified date.
%% If used by humans, please run mnesia:start() first.
%% @end 
list_campaign_counters(Date) ->
    F = fun() -> mnesia:select(counter,
                               ets:fun2ms(fun (#counter{key=#campaign_stat{date=D}})
                                              when D =:= Date ->
                                                  object()
                                          end))
        end,
    {atomic,List}= mnesia:transaction(F),
    List.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @spec foreach_eq(Fun::function(), To::term(), list()) -> any()
%% @doc Applies Fun to each item in the list and ensures the result is
%%      equal to the term To.
%% @end
foreach_eq(Fun, EqTo, List) when is_function(Fun, 1), is_list(List) ->
    lists:foreach(fun (I) ->
                          true = EqTo =:= Fun(I)
                  end,
                  List).

%%====================================================================
%% EUnit tests
%%====================================================================
foreach_eq_test() ->
    foreach_eq(fun (_) -> ok end, ok, lists:seq(1,10)),
    ?assert(true),
    ?assertError(_, foreach_eq(fun (A) when A =< 9 -> ok end, ok, lists:seq(1,10))).

