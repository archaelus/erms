%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ERMS Reporting library
%% @end
%%%-------------------------------------------------------------------
-module(erms_reports).

-include_lib("logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("mnesia_model.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("esmtp_mime.hrl").

%% API
-compile(export_all).

-record(delivery_stat, {date,
                        node,
                        shortcode,
                        connection,
                        network,
                        state}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================

format_delivery_stats(Stats) ->
    ["Date, Node, Shortcode, Connection, Prefix, State, MsgCount\n"|
     lists:map(fun format_delivery_stat/1,
               Stats)].

descending(Stats) ->
    lists:reverse(lists:keysort(1, Stats)).

format_delivery_stat({#delivery_stat{date={Year, Month, Day},
                                     node=Node,
                                     shortcode=Shortcode,
                                     connection=C,
                                     network=Prefix,
                                     state=State},
                      Count}) ->
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B, ~p, ~s, ~s, ~s, ~p, ~p~n",
                  [Year, Month, Day, Node, Shortcode, C, Prefix, State, Count]).

find_unarchived_dates() ->
    F = fun () ->
                Dates = lists:map(fun (U) ->
                                          {D,_} =  erms_uuid:utc_datetime(U),
                                          D
                                  end,
                                  mnesia:all_keys(msg_status)),
                lists:usort(Dates)
        end,
    {atomic, Dates} = mnesia:transaction(F),
    Dates.

msg_date(Id) ->
    {Date, _Time} = calendar:now_to_local_time(erms_uuid:nowtime(Id)),
    Date.

today() ->
    {Date, _Time} = calendar:now_to_local_time(erlang:now()),
    Date.

archived_msgs() ->
    mnesia:select(archived_msg,
                  ets:fun2ms(fun (#archived_msg{msgs=M,
                                                msg_status=S}) ->
                                     {S, M}
                             end)).

archived_msgs(Date) ->
    mnesia:select(archived_msg,
                  ets:fun2ms(fun (#archived_msg{archive_date=D,
                                                msgs=M,
                                                msg_status=S}) when D == Date ->
                                     {S, M}
                             end)).

%% Calculate delivery statistics from archived messages.
delivery_stats(Messages) when is_list(Messages) ->
    lists:foldl(fun (I, D) ->
                        update_counters(D, delivery_stats(I))
                end,
                dict:new(),
                Messages);
delivery_stats({#msg_status{targets=T, state=State, msg_id=MsgId,
                            shortcode=Shortcode},
                Messages}) when State == expired; State == delivered ->
    {Date, _Time} = calendar:now_to_local_time(erms_uuid:nowtime(MsgId)),
    FindMsgNet = fun (MId) -> 
                         NetMsg = erms_msg:fetch(MId, Messages),
                         case {erms_numbers:sparse(NetMsg#msg.to),
                               erms_numbers:sparse(NetMsg#msg.from)}
                             of
                             {{_, Net, _}, _} -> "0" ++ Net;
                             {_, {_, Net, _}} -> "0" ++ Net;
                             _Else -> "Unknown"
                         end
                 end,
    case T of
        % Failed route
        undefined ->
            [{#delivery_stat{date=Date,
                             node=erms_uuid:node(MsgId),
                             shortcode=Shortcode,
                             connection="Unroutable",
                             network="Unknown",
                             state=State}, 1}];
        _ ->
            [{#delivery_stat{date=Date,
                             node=erms_uuid:node(MsgId),
                             shortcode=Shortcode,
                             connection=C,
                             network=FindMsgNet(Id),
                             state=State}, 1}
             || {Id, {connection, C}} <- T]
    end;
% Old msg_status format (< erms-0.7)
delivery_stats({{msg_status,MSG_ID,FROM_CONNECTION,DIRECTION,SHORTCODE,TARGETS,STATE},
                Messages}) ->
    delivery_stats({#msg_status{msg_id=MSG_ID,
                                from_connection=FROM_CONNECTION,
                                direction=DIRECTION,
                                shortcode=SHORTCODE,
                                targets=TARGETS,
                                state=STATE}, Messages}).

%% Function: update_counters
%% @spec (Dict::dict(), Counters::Ctrs) -> dict()
%%   Ctrs = [{CounterName::term(),Value::number()}]
%% @doc Updates a list of counters in a dictionary.
%% @end
update_counters(Dict, Counters) ->
    lists:foldl(fun ({K, V}, Acc) ->
                        dict:update_counter(K, V, Acc)
                end,
                Dict,
                Counters).

%% Function: merge_counters
%% @spec (D1::dict(), D2::dict()) -> dict()
%% @doc Merges two dictionaries containing counters, adding values for shared keys.
%% @end
merge_counters(D1, D2) ->
    dict:merge(fun (_, V1, V2) -> V1 + V2 end,
               D1, D2).

%% Function: update_delivery_stats
%% @spec (NewStats::dict()) -> ok | {error, Reason::term()}
%% @doc Updates the report_data mnesia table with new delivery statistics.
%%   Needs to be called within an mnesia:transaction.
%%   Works by overwriting the counters in the counter table with the values
%%   from the dictionary. Probably only useful when recalculating stats
%%   from the message archive table.
%% @end
update_delivery_stats(NewStats) ->
    lists:foreach(fun mnesia:write/1,
                  [#counter{key=K, count=V}
                   || {K,V} <- dict:to_list(NewStats)]).


stat_dates() ->
    mnesia:select(counter,
                  ets:fun2ms(fun (#counter{key=#delivery_stat{date=Date,_='_'}, count='_'}) ->
                                     Date
                             end)).

stats_for_date(Date) ->
    mnesia:select(counter,
                  ets:fun2ms(fun (#counter{key=K, count=C})
                                 when K#delivery_stat.date == Date ->
                                     {K,C}
                             end)).

stats_for_shortcode(Code) ->
    mnesia:select(counter,
                  ets:fun2ms(fun (#counter{key=K, count=C})
                                 when K#delivery_stat.shortcode == Code ->
                                     {K,C}
                             end)).

complete_stats() ->
    mnesia:select(counter,
                  ets:fun2ms(fun (#counter{key=K, count=C}) ->
                                     {K,C}
                             end)).

stats_for_node_t() ->
    mnesia:transaction(fun stats_for_node/0).

stats_for_node() ->
    stats_for_node(node()).

stats_for_node(Node) ->
    mnesia:select(counter,
                  ets:fun2ms(fun (#counter{key=K, count=C})
                                 when K#delivery_stat.node == Node ->
                                     {K,C}
                             end)).

shortcode_report(Code) ->
    {atomic, Stats} = mnesia:transaction(fun stats_for_shortcode/1, [Code]),
    format_delivery_stats(descending(Stats)).

mime_report(Shortcode, Report, Receipients, Body) ->
    M = mail_mime:msg(Receipients, "erms@" ++ net_adm:localhost(),
                      report_subject(Shortcode)),
    Attachment = #mime_part{data=Report,
                            type=attachment,
                            encoding={"8bit", "text/csv","utf-8"},
                            name=quote(report_filename(Shortcode))},
    BodyPart = #mime_part{data=Body},
    M#mime_msg{parts=[BodyPart,Attachment]}.

report_filename(Shortcode) ->
    {Year, Month, Day} = today(),
    io_lib:format("~s delivery report ~4.10.0B-~2.10.0B-~2.10.0B.csv",
                  [Shortcode, Year, Month, Day]).

report_subject(Subj) ->
    lists:flatten(io_lib:format("[~s] ERMS Delivery Statistics Report", [Subj])).

shortcode_report_body(Shortcode) ->
    B = io_lib:format("The delivery statistics report for the ~s shortcode~n"
                      "is attached to this message.~n~n", [Shortcode]),
    lists:flatten(B).

complete_report_body() ->
    B = io_lib:format("The delivery statistics report ~n"
                      "is attached to this message.~n~n", []),
    lists:flatten(B).

mail_shortcode_report(Shortcode, Receipients) ->
    mail_shortcode_report(Shortcode, Receipients, shortcode_report_body(Shortcode)).

mail_shortcode_report(Shortcode, Receipients, Body) ->
    erlmail_relay:send(mime_report(Shortcode,
                                   shortcode_report(Shortcode),
                                   Receipients,
                                   Body)).

mail_complete_report(Receipients) ->
    {atomic, Stats} = mnesia:transaction(fun complete_stats/0),
    Report = format_delivery_stats(descending(Stats)),
    erlmail_relay:send(mime_report("All",
                                   Report,
                                   Receipients,
                                   complete_report_body())).

% Call a given node for its statistics, then delete our local copies
% of statistics for that node before writing the results.
repopulate_from_node(Node) when is_atom(Node) ->
    case rpc:call(Node, ?MODULE, stats_for_node_t, []) of
        {atomic, Stats} ->
            NewStats = lists:filter(fun ({#delivery_stat{node=N}, V}) when N == Node -> true;
                                        (_) -> false
                                    end,Stats),
            mnesia:transaction(fun () ->
                                       OldStats = stats_for_node(Node),
                                       delete_stats(OldStats),
                                       foreach_eq(fun mnesia:write/1, ok, NewStats),
                                       ok
                               end)
    end.

% Here we use a sticky_write because we're currently only called from
% repopulate_from_node/1 Where the algorithm is delete all stats for
% node N, write all stats from node N.  As we're usually going to
% write back everything we delete, a sticky_write should be faster if
% there's more than one node in our schema for the counter
% table. *theory*
delete_stats(Stats) ->
    lists:foreach(fun (S) ->
                          ok = mnesia:delete(counter, S#counter.key, sticky_write)
                  end,
                  Stats).

quote(String) ->
    "\"" ++ String ++ "\"".

foreach_eq(Fun, EqTo, List) ->
    lists:foreach(fun (I) ->
                          Fun(I) == EqTo
                  end,
                  List).

crosspopulate_all_nodes() ->
    erms_rpc:call_with_other_nodes(fun ?MODULE:repopulate_from_node/1).
