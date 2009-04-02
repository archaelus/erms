%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc SMPP utility functions
%% @end
%%%-------------------------------------------------------------------
-module(erms_smpp).

%% API
-export([pdu_to_msg/1,
         smpptime_year_normalise/1,
         pdu_timing/1,
         smpp_timing/2,
         smpptime_to_universal/1,
         smpptime_to_universal/2,
         universal_to_smpptime/1,
         msg_to_pdu/1,
         msg_to_pdu/2]).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include_lib("eunit.hrl").
-include_lib("oserl.hrl").
-include_lib("smpp_base.hrl").
-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------

%% @spec pdu_to_msg(Pdu::dict()) -> #msg{}
%% @doc Extracts an erms #msg{} from an oserl pdu.
%% @end 
pdu_to_msg(Pdu) ->
    From = dict:fetch(source_addr, Pdu),
    To = dict:fetch(destination_addr, Pdu),
    Text = dict:fetch(short_message, Pdu),
    erms_msg:msg(From, To, Text).

pdu_timing(Pdu) ->
    RawDelivery = extract_delivery(Pdu),
    RawExpiry = extract_expiry(Pdu),
    smpp_timing(RawDelivery, RawExpiry).

smpp_timing(undefined, undefined) ->
    {undefined, undefined};
smpp_timing(undefined, RawExpiry) ->
    {undefined, smpptime_to_universal(RawExpiry)};
smpp_timing(RawDelivery, undefined) ->
    {smpptime_to_universal(RawDelivery), undefined};
smpp_timing(RawDelivery, RawExpiry) ->
    DeliveryDT = smpptime_to_universal(RawDelivery),
    {DeliveryDT,
     adjust_expiry(DeliveryDT,
                   smpptime_to_universal(RawExpiry))}.

adjust_expiry(BaseDT, ExpiryDT) ->
    adjust_expiry(BaseDT, ExpiryDT, erlang:universaltime()).

adjust_expiry(BaseDT, ExpiryDT, NowDT) ->
    datetime_add(BaseDT, datetime_diff(NowDT, ExpiryDT)).

extract_expiry(Pdu) ->
    case dict:find(validity_period, Pdu) of
        {ok, Time} when Time /= [] ->
            Time;
        _ ->
            undefined
    end.

extract_delivery(Pdu) ->
    case dict:find(schedule_delivery_time, Pdu) of
        {ok, Time} when Time /= [] ->
            Time;
        _ ->
            undefined
    end.

smpptime_to_universal(Raw) ->
    smpptime_to_universal(Raw, erlang:universaltime()).

smpptime_to_universal([Yh,Yl, Mh,Ml, Dh, Dl,
                       Hh, Hl, Mih, Mil, Sh, Sl, _St,
                       _Nh, _Nl, $R], NowDT) ->
    [_Year, _Month, Day, Hour, Min, Sec] =
        smpptime_parse([Yh,Yl],[Mh,Ml],[Dh,Dl],
                       [Hh, Hl],[Mih, Mil], [Sh,Sl]),
    % How many days are there in a month? or a year?
    Secs = (Day * 24 * 60 * 60) + (Hour * 60 * 60) + (Min * 60) + Sec,
    datetime_add(NowDT, Secs);
smpptime_to_universal([Yh,Yl, Mh,Ml, Dh, Dl,
                       Hh, Hl, Mih, Mil, Sh, Sl, St,
                       Nh, Nl, P], _NowDT) ->
    Time = smpptime_parse([Yh,Yl],[Mh,Ml],[Dh,Dl],
                          [Hh, Hl],[Mih, Mil], [Sh,Sl], St),
    DT = list_to_datetime(Time),
    smpptime_adjust(DT, [P,Nh,Nl]).

smpptime_parse(Year, Month, Day, Hour, Min, Sec) ->
    [list_to_integer(Year),
     list_to_integer(Month),
     list_to_integer(Day),
     list_to_integer(Hour),
     list_to_integer(Min),
     list_to_integer(Sec)].

smpptime_parse(Year, Month, Day, Hour, Min, Sec, Tenths) ->
    [list_to_integer(Year),
     list_to_integer(Month),
     list_to_integer(Day),
     list_to_integer(Hour),
     list_to_integer(Min),
     round(list_to_float(Sec ++ [$.,Tenths]))].

list_to_datetime([Year, Month, Day, Hour, Min, Sec]) ->
    {{smpptime_year_normalise(Year), Month, Day}, {Hour, Min, Sec}}.

smpptime_adjust(DT, [_,$0,$0]) ->
    DT;
smpptime_adjust(DT, TZoffset) ->
    TZo = list_to_integer(TZoffset),
    TZoSecs = 60 * 15 * TZo,
    AdjSecs = calendar:datetime_to_gregorian_seconds(DT) - TZoSecs,
    calendar:gregorian_seconds_to_datetime(AdjSecs).

smpptime_year_normalise(Yr) when Yr > 200 ->
    Yr;
smpptime_year_normalise(Yr) when Yr > 69 ->
    1900 + Yr;
smpptime_year_normalise(Yr) ->
    2000 + Yr.

datetime_add(T, Interval) ->
    S = calendar:datetime_to_gregorian_seconds(T),
    calendar:gregorian_seconds_to_datetime(S + Interval).

datetime_diff(T1, T2) ->
    calendar:datetime_to_gregorian_seconds(T2) -
        calendar:datetime_to_gregorian_seconds(T1).

universal_to_smpptime({{Year,Month,Day},{Hour,Min,Sec}}) ->
    Fmt = "~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B000+",
    lists:flatten(io_lib:format(Fmt,
                                [Year, Month, Day, Hour, Min, Sec])).

msg_to_pdu(Msg = #msg{}) ->
    msg_to_pdu(Msg, []).

msg_to_pdu(#msg{from=From,to=To,text=Text}, Options) ->
    PduOptions = operation:merge_params(Options,
                                        sm_default_pdu()),
    operation:merge_params([{source_addr, From},
                            {destination_addr, To},
                            {short_message, Text}],
                           PduOptions).

sm_default_pdu() ->
    [{service_type, ?SERVICE_TYPE_NULL},
     {source_addr_npi, ?NPI_ISDN},
     {dest_addr_npi, ?NPI_ISDN},
     {source_addr_ton, ?TON_INTERNATIONAL},
     {dest_addr_ton, ?TON_INTERNATIONAL},
     {esm_class, ?ESM_CLASS_MODE_STORE_FORWARD},
     {protocol_id, ?PROTOCOL_IDENTIFIER_GSM},
     {priority_flag, ?PRIORITY_FLAG_GSM_SMS_NON_PRIORITY},
     {schedule_delivery_time, ?SCHEDULE_DELIVERY_TIME_IMMEDIATE},
     {validity_period, ?VALIDITY_PERIOD_DEFAULT},
     {registered_delivery, ?REGISTERED_DELIVERY_DEFAULT},
     {replace_if_present_flag, ?REPLACE_IF_PRESENT_FLAG_DO_NOT_REPLACE},
     {data_coding, ?ENCODING_SCHEME_MC_SPECIFIC}].

%%====================================================================
%% Internal functions
%%====================================================================

smpptime_year_normalise_test() ->
    ?assert(smpptime_year_normalise(0) == 2000),
    ?assert(smpptime_year_normalise(70) == 1970),
    ?assert(smpptime_year_normalise(1970) == 1970),
    ?assert(smpptime_year_normalise(2000) == 2000),
    ?assert(lists:all(fun (N) -> 
                              smpptime_year_normalise(N) >= 1970
                      end,
                      lists:seq(0,170))).
    

smpptime_relative_test() ->
    Now = {{2007,05,17},{15,58,00}},
    ?assert(smpptime_to_universal("000000000000000R",
                                  Now) ==
            {{2007,05,17},{15,58,00}}),
    ?assert(smpptime_to_universal("000000000200000R",
                                  Now) ==
            {{2007,05,17},{16,00,00}}),
    ?assert(smpptime_to_universal("000000010301000R",
                                  Now) ==
            {{2007,05,17},{17,01,01}}),
    ?assert(smpptime_to_universal("000019010301000R",
                                  Now) ==
            {{2007,06,05},{17,01,01}}),
    ?assert(smpptime_to_universal("000000000001000R",
                                  Now) ==
            {{2007,05,17},{15,58,01}}).

smpptime_absolute_test() ->
    ?assertMatch({{2007,05,17},{15,58,00}},
                 smpptime_to_universal("070517155800000+")),
    ?assertMatch({{2007,05,17},{15,58,00}},
                 smpptime_to_universal("070517155800000-")),
    ?assertMatch({{2007,5,17},{3,58,0}},
                 smpptime_to_universal("070517155800048+")).

