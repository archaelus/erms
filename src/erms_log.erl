%%% File    : erms_log.erl
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%%% Author  : Geoff Cant <nem@erlang.geek.nz>
%%% Description : Logging API for ERMS.
%%% Created :  5 Jun 2006 by Geoff Cant <nem@erlang.geek.nz>


-module(erms_log).

-export([error/5, warn/5, info/5]).

error(Module, LineNo, Pid, Format, Data) ->
    error_logger:error_msg(lists:append("(~p ~p:~p) ", Format), [Pid, Module, LineNo|Data]).
%    erms_logger:log(error, Module, LineNo, Pid, Format, Data).

warn(Module, LineNo, Pid, Format, Data) ->
    error_logger:warning_msg(lists:append("(~p ~p:~p) ", Format), [Pid, Module, LineNo|Data]).
%    erms_logger:log(warning, Module, LineNo, Pid, Format, Data).

%info(Module, _, _, _, _) when Module == erms_esme_connection; Module == erms_http_connection; Module == erms_debug_connection; Module == erms_shortcode -> 
%    ok;
info(Module, LineNo, Pid, Format, Data) ->
    error_logger:info_msg(lists:append("(~p ~p:~p) ", Format), [Pid, Module, LineNo|Data]).
%    erms_logger:log(info, Module, LineNo, Pid, Format, Data).
