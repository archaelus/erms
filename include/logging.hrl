%%% File    : erms_logging.hrl
%%% Author  : Geoff Cant <nem@lisp.geek.nz>
%%% Description : Logging macros
%%% Created : 13 Jan 2006 by Geoff Cant <nem@lisp.geek.nz>

-ifndef(erms_logging).
-define(erms_logging, true).

-define(INFO(Format, Args),
        erms_log:info(?MODULE, ?LINE, self(), Format, Args)).
-define(WARN(Format, Args),
        erms_log:warn(?MODULE, ?LINE, self(), Format, Args)).
-define(ERR(Format, Args),
        erms_log:error(?MODULE, ?LINE, self(), Format, Args)).

-endif. %erms_logging
