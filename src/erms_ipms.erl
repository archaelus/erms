%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Industry Portability Management System report access library
%% @end
%%%-------------------------------------------------------------------
-module(erms_ipms).
-include_lib("logging.hrl").
-include_lib("eunit.hrl").

%% API
-export([read_report_file/1,
         read_report/1,
         parse_pmn_line/1,
         report_consistent/1,
         dict/1,
         dict/2]).

%%====================================================================
%% API
%%====================================================================
%% @type ipms_rpt() = list(RptItem)
%%  RptItem = {report_header, {calendar:date(), calendar:time()}, RecordCount::integer()} +
%%            {report_footer, RecordCount::integer()} +
%%            {port, MobileNumber::string(), CarrierId::string(), CarrierName::string()}.
%% An IPMS report (with headers/footers).
%% @end

%%--------------------------------------------------------------------
%% @spec read_report_file(FileName::string()) -> Report::ipms_rpt()
%% @doc Reads an IPMS ported mobile number extract from the given
%%   filename.
%% @end 
read_report_file(FName) ->
    {ok, File} = file:open(FName, [read, read_ahead]),
    Rpt = read_report(File),
    file:close(File),
    Rpt.


%% @spec read_report(IoDevice) -> ipms_rpt()
%% @doc Reads an IPMS report from an io device.
%% @end
read_report(File) ->
    read_report(File, []).

read_report(File, Acc) ->
    case io:get_line(File, []) of
        eof ->
            Acc;
        String ->
            read_report(File, [parse_pmn_line(String)|Acc])
    end.

parse_pmn_line([Y1,Y2,Y3,Y4,$/,M1,M2,$/,D1,D2,$\s,
                H1,H2,$:,Mi1,Mi2,$:,S1,S2,$,|RecCount]) ->
    Year = list_to_integer([Y1,Y2,Y3,Y4]),
    Month = list_to_integer([M1,M2]),
    Day = list_to_integer([D1,D2]),
    Hour = list_to_integer([H1,H2]),
    Min = list_to_integer([Mi1,Mi2]),
    Sec = list_to_integer([S1,S2]),
    {report_header, {{Year,Month,Day},{Hour,Min,Sec}},
     list_to_integer(chomp(RecCount))};
parse_pmn_line("\"<EOF>\"," ++ RecCount) ->
    {report_footer, list_to_integer(chomp(RecCount))};
parse_pmn_line(PortLine) ->
    [Number,CarrierId,Carrier] = string:tokens(PortLine,","),
    {port, Number, CarrierId, chomp(Carrier)}.

%% @spec report_consistent(ipms_rpt()) -> bool()
%% @doc Examines the footer of the IPMS report and returns true if the
%%   report contains the same number of records as the footer
%%   specifies.
%% @end
report_consistent(Rpt) ->
    {report_footer, Count} = hd(Rpt),
    Count + 2 == length(Rpt).


dict(Rpt) ->
    case application:get_env(erms, carrier_map) of
        undefined -> exit(missing_carrier_map);
        CarrierMap ->
            dict(Rpt, CarrierMap)
    end.

%% @spec dict(ipms_rpt(), CarrierMap::list()) -> dict:dictionary()
%% @doc Creates a Number -> Carrier dictionary based on an IPMS report
%%   and a proplist that maps carrier names to carrier atoms (e.g.
%%   [{"Telecom", telecom},{"Vodafone", vodafone}]).
%% @end
dict(Rpt, CarrierMap) ->
    C = fun (Carrier) ->
                proplists:get_value(Carrier, CarrierMap, unknown_carrier)
        end,
    dict:from_list([{erms_numbers:sparse(Number), C(Carrier)}
                    || {port, Number, _, Carrier} <-Rpt]).

%%====================================================================
%% Internal functions
%%====================================================================

chomp(String) ->
    string:strip(String, right, $\n).

%%====================================================================
%% EUnit tests
%%====================================================================

parse_pmn_test() ->
    ?assertMatch({report_header, {{2007,5,30},{0,10,0}}, 791},
                 parse_pmn_line("2007/05/30 00:10:00,791")),
    ?assertMatch({port, "0211071000","99917","FBNVodafoneMob"},
                 parse_pmn_line("0211071000,99917,FBNVodafoneMob")),
    ?assertMatch({report_footer, 791},
                 parse_pmn_line("\"<EOF>\",791")).
    
