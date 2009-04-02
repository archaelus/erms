%%% File    : mnesia_model.hrl
%%% Author  : Geoff Cant <nem@lisp.geek.nz>
%%% Description : ERMS Datastructures
%%% Created : 04 Oct 2006 by Geoff Cant <nem@lisp.geek.nz>

-ifndef(erms_model).
-define(erms_model, true).

-record(user, {name,
               password,
               realname,
               email}).

-record(shortcode, {name,
                    description,
                    mt_rules,
                    mo_rules}).

-record(connection, {name,
                     mod,
                     args,
                     supervised,
                     rules}).

-record(login, {type,
                username,
                password,
                shortcode_name,
                connection_name}).

-record(msg, {id,
              from,
              to,
              text}).

-record(msg_status, {msg_id, % erms_uuid
                     from_connection, % #connection.name
                     direction, % mo | mt
                     shortcode, % #shortcode.name
                     targets = [], % {#msg.id, Connection} pairs
                     state = new,
                     delivery_time, % delay initial delivery until
                     expiry_time % expire after
                    }).

-define(MSG_STATES, [new, in_progress, expired, delivered]).

-record(delivery_failure, {orig_msg_id,
                           msg_id,
                           destination,
                           time,
                           reason}).

-record(delivery_success, {orig_msg_id,
                           msg_id,
                           destination,
                           time}).

-record(report_history, {date,
                         report,
                         data}).

-record(archived_msg, {msg_id,
                       archive_date,
                       version = 2,
                       msg_status,
                       msgs,
                       delivery_details}).

-record(counter, {key, count}).

-record(campaign, {name, triggers, ad}).

-define(ERMS_HIGH_VOLUME_TABLES, [msg, msg_status, delivery_success, delivery_failure, archived_msg]).

-endif. %erms_model
