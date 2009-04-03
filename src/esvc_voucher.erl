%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Vik Olliver <vik@catalyst.net.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Voucher parsing code for adding vouchers to 018 SMS messages.
%% @end
%%%-------------------------------------------------------------------
-module(esvc_voucher).

-include_lib("eunit/include/eunit.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("esvc_voucher.hrl").

%% API
-export([process_mt/2]).
-export([process_campaign/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec process_mt(Msg, Options) -> term()
%%    Msg = #msg{}
%%    Options = list(Option)
%%    Option = {string_to_replace, string()}
%% @doc Given a string_to_replace, and list of ad campaigns, look for the
%%      campaign's phone number in the message and if found replace the
%%      string with the campaign's advert. Campaign record format in
%%      mnesia_model.hrl
%% @end
process_mt(Msg = #msg{}, Options) when is_list(Options) ->
    process_campaigns(Msg, get_campaigns(Options), Options).

%% @spec process_campaigns(Msg, Campaigns, Options) -> term()
%%    Msg = #msg{}
%%    Campaigns = list()
%%    Options = list()
%% @doc Checks if a message is affected by a campaign, and if it is appends the relevant advert.
%% @end
process_campaigns(Msg, Campaigns, Options) when is_list(Campaigns) ->
    process_campaign(Msg, find_campaign(Msg#msg.text, Campaigns), Options).

%% @private
%% @spec find_campaign(Message, list(Campaign)) -> {Campaign,Trigger}
%%    Message = string() | #msg{}
%% @doc Foo.
%% @end
find_campaign(#msg{text=Text}, Campaigns) ->
    find_campaign(Text, Campaigns);
find_campaign(_Text, []) -> no_campaign;
find_campaign(Text, [C = #campaign{triggers=Ts} | Cs]) ->
    case test_triggers(Text, Ts) of {true,Expr} -> {C,Expr};
         false -> find_campaign(Text, Cs)
    end.

test_triggers(Text, [Regex|Tail]) ->
    case has_match(Text,Regex) of
        true ->
            {true,Regex};
        false ->
            test_triggers(Text,Tail)
    end;
test_triggers(_Text, []) ->
    false.

process_campaign(Msg, no_campaign, _) ->
    Msg;
process_campaign(M=#msg{text=Body}, {#campaign{name=Name, ad=Ad},Trigger}, Options) ->
    TextToReplace = proplists:get_value(string_to_replace, Options,"Delivered by 018."),
    % If target text not found, just append advert.
    update_stats(Name, Trigger, Options),
    case re:replace(Body, TextToReplace, Ad) of
        T when T =:= Body -> M#msg{text=lists:append([Body, " ", Ad])};
        NewText -> M#msg{text=NewText}
    end.

%% @private
%% @doc Log this hit by date, campaign name and trigger.
update_stats(CampaignName, Trigger, Options) when is_list(Options) ->
    update_stats(CampaignName, Trigger, proplists:get_value(stats, Options, true));
update_stats(_, _, false) ->
    ok;
update_stats(CampaignName,Trigger, true) ->
    {Date, _Time} = calendar:universal_time(),
    Key=#campaign_stat{date=Date,name=CampaignName,trigger=Trigger},
    mnesia:dirty_update_counter(counter,Key,1).

%% @private
%% @spec get_campaigns(Options::list()) -> lists(Campaign)
%% @doc Retreives the campagins from mnesia.
%% @end
get_campaigns(_Options) ->
    dummy_campaigns().

%% @spec has_match(string(), Regex::string()) -> boolean()
has_match(String, Regex) ->
    case re:run(String, Regex) of
        {match, _} -> true;
        nomatch -> false
    end.
%%====================================================================
%% Internal functions
%%====================================================================

%% @private
dummy_campaigns() ->
	[
	#campaign{name="Campaign test 1", triggers=["0912345678"], ad="Get numerate today!"},
	#campaign{name="Campaign test 2", triggers=["0987654321"], ad="Get numerate tomorrow!"},
	#campaign{name="Campaign test 3", triggers=["0969696969"], ad="Get numerate eventually!"},
	#campaign{name="Campaign test 4", triggers=["098177136","044444444"], ad="fECK LITERUCY!"}
	].

%% @private
test_process_campaigns(Message, Campaigns, ExpectedMsgText) ->
    Result=process_campaigns(#msg{text=Message}, Campaigns,
                             [{string_to_replace, "Delivered by 018."},
                              {stats, false}]),
    Text=Result#msg.text,
    (ExpectedMsgText==Text).

%%====================================================================
%% Test functions
%% @todo Will fail as DB doesn;t exist, so counter can't be set.
%%====================================================================

%% @private
vouchers_test_() ->
    [?_assert(test_process_campaigns("A text message with no hits",
        dummy_campaigns(),"A text message with no hits")),
     ?_assert(test_process_campaigns("A text message with a hit (0987654321) but no key.",
    dummy_campaigns(),
    "A text message with a hit (0987654321) but no key. Get numerate tomorrow!")),
    ?_assert(test_process_campaigns("A text message with a hit (0969696969) and a key. Delivered by 018.",
    dummy_campaigns(),
    "A text message with a hit (0969696969) and a key. Get numerate eventually!"))
    ].
