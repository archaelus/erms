%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Vik Olliver <vik@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Voucher CVS file export code
%% @end
%%%-------------------------------------------------------------------
-module(esvc_voucher_export).
-include_lib("eunit/include/eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("esvc_voucher.hrl").

%% API
-export([cvs_report/2]).


%%====================================================================
%% API
%%====================================================================


%% @spec cvs_report(Filename,Date) -> [ListOfCampaigns]
%% @doc Reads a Campaign's counter statistics for the specified
%% Date and destructively puts them in CVS format in the file Filename.
%% @end
cvs_report(Filename,Date) ->
    {ok, File} = file:open(Filename, write),
    Campaigns = esvc_voucher_db:list_campaign_counters(Date),
    lists:foreach(fun (Camp) ->
                          Rec=Camp#counter.key,
                          [Trig|_]=Rec#campaign_stat.trigger,
                          {Year,Month,Day}=Rec#campaign_stat.date,
                          io:format(File,"~p-~p-~p, ~p, ~p, ~p~n",
                                    [Year,Month,Day,Rec#campaign_stat.name,Trig,Camp#counter.count])
                  end,
                  Campaigns),
    file:close(File).

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% EUnit tests
%%====================================================================

