%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Vik Olliver <vik@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Voucher CVS file import code
%% @end
%%%-------------------------------------------------------------------
-module(esvc_voucher_import).
-include_lib("eunit/include/eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").

%% API
-export([read_campaign_file/1,
         campaign/3,
         parse_line/1]).


%%====================================================================
%% API
%%====================================================================


%%--------------------------------------------------------------------
%% @spec read_campaign_file(FileName::string()) -> [#campaign{}]
%% @doc Reads a CSV Campaign file into an Mnesia database ready for use
%% by voucher code.
%% @end 
read_campaign_file(FName) when is_list(FName) ->
    {ok, File} = file:open(FName, [read, read_ahead]),
    read_campaign_file(File, []).

%% @private
%% @spec read_campaign_file(IoDevice, Acc) -> [#campaign{}]
%%   Acc = [#campaign{}]
%% @doc Reads a CSV Campaign from an io device.
%% @end
read_campaign_file(File, Acc) ->
    case io:get_line(File, []) of
        String when is_list(String) ->
            Campaigns = parse_line(String),
            read_campaign_file(File, lists:append(Campaigns, Acc));
        _Error ->
            file:close(File),
            Acc
    end.

%% @spec parse_line(string()) -> [#campaign{}]
%% @doc Parses a string of CSV data and returns a list of campaigns from it.
parse_line(String) ->
    parse_csv(esvc_csv:read(String)).

%% @private
parse_csv(List) ->
    parse_csv(List, []).

%% @private
parse_csv([], Acc) -> Acc;
parse_csv([[]|Rest], Acc) ->
    parse_csv(Rest, Acc);
parse_csv([[Name,Trigger,Ad]|Rest], Acc) ->
    parse_csv(Rest, [#campaign{name=Name, triggers=[Trigger], ad=Ad}|Acc]);
parse_csv([Campaign|Rest], Acc) when length(Campaign) > 3 ->
    Len = length(Campaign),
    parse_csv(Rest, [#campaign{name=hd(Campaign), 
                               triggers=lists:sublist(Campaign, 2, (Len-2)),
                               ad=lists:last(Campaign)}|Acc]);
parse_csv([BogusCampaign|Rest], Acc) ->
    ?WARN("Couldn't read campaign data - ~p", [BogusCampaign]),
    parse_csv(Rest, Acc).

%% @private
%% @spec campaign(Name::string(), Triggers, Ad::string()) -> #campaign{}
%%    Triggers = [string()]
%% @doc Campaign constructor
%% @end
campaign(Name, Triggers, Ad) ->
    #campaign{name=Name, triggers=Triggers, ad=Ad}.

%%====================================================================
%% EUnit tests
%%====================================================================

parse_line_test() ->
    LineA = "hello, there, world\n",
    ParsedA=#campaign{name="hello",triggers=["there"],ad="world"},
    LineB = "Campaign name,091234567,098177138,\"Sometimes, the world is not enough\"\n",
    ParsedB=#campaign{name="Campaign name",
                      triggers=["091234567",
                                "098177138"],
                      ad="Sometimes, the world is not enough"},
    ?assert(parse_line(LineA) == [ParsedA]),
    ?assert(parse_line(LineB) == [ParsedB]).

parse_line2_test() ->
    Line = "hello, there, world\nCampaign name,091234567,098177138,Sometimes, the world is not enough\n",
    Campaigns = [#campaign{name="hello",triggers=["there"],ad="world"},
                 #campaign{name="Campaign name",
                           triggers=["091234567",
                                     "098177138",
                                     "Sometimes"],
                           ad="the world is not enough"}],
    Parsed = parse_line(Line),
    ?assert(lists:sort(Campaigns) == lists:sort(Parsed)).

parse_csv_test() ->
    CSVData = [["hello","there","world"],
               ["Campaign name",
                "091234567",
                "098177138",
                "Sometimes",
                "the world is not enough"],
               ["Foo"]],
    Campaigns = [#campaign{name="hello",triggers=["there"],ad="world"},
                 #campaign{name="Campaign name",
                           triggers=["091234567",
                                     "098177138",
                                     "Sometimes"],
                           ad="the world is not enough"}],
    Parsed = parse_csv(CSVData),
    ?assert(lists:sort(Campaigns) == lists:sort(Parsed)).
