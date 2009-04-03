%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn} {@date} {@time}
%% @doc MNP MO service (300) application code.
%% @end
%%%-------------------------------------------------------------------
-module(esvc_mnp_mo).

-include_lib("logging.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([reply/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (Msg, Options::List) -> Reply::Msg
%%  Msg = #msg{}
%% @doc Calculates a reply for the MNP MO service.
%% @end 

reply(#msg{text=Text,from=From,to=To},Options) ->
    Invalid = proplists:get_value(invalid, Options),
    Telecom = proplists:get_value(telecom, Options),
    NotTelecom = proplists:get_value(not_telecom, Options),
    Error = proplists:get_value(error, Options, Invalid),
    MnpOptions = proplists:get_value(mnp_options, Options),
    NumberFormat = proplists:get_value(number_format, Options),
    case erms_numbers:sparse(Text) of
        {error, _} -> erms_msg:msg(To,From,fmt(Invalid,[]));
        Number ->
            case erms_mnp:mnp_lookup(Number, MnpOptions) of
                invalid -> erms_msg:msg(To,From,fmt(Invalid,[]));
                telecom -> erms_msg:msg(To,From,fmt(Telecom,[erms_numbers:format(NumberFormat,Number)]));
                not_telecom -> erms_msg:msg(To,From,fmt(NotTelecom,[erms_numbers:format(NumberFormat,Number)]));
                Error -> erms_msg:msg(To,From,fmt(Error,[]))
            end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

fmt(FmtStr, Args) ->
    lists:flatten(io_lib:format(FmtStr, Args)).
