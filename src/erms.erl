%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc This module contains functions intended to be run by humans
%%  from a console node.
%%  This module also contains various utility code that should migrate
%%  to more appropriate modules in the fullness of time.
%% @end
%%%-------------------------------------------------------------------
-module(erms).

-include_lib("mnesia_model.hrl").
-include_lib("logging.hrl").
-include_lib("eunit.hrl").

%% API
-export([shortcode_msgs/1,
         shortcode_msgs/2,
         reconfigure_shortcode/1,
         delivery_times/0,
         delivery_times/1,
         inject/5,
         inject/6,
         connections/0,
         shortcodes/0,
         logins/0,
         connection/1,
         shortcode/1,
         trace/1,
         untrace/0,
         reload_code/0,
         msg/3,
         sync_deliver/2,
         source_of/1,
         print_source_of/1,
         dbg/0,
         p/4,
         msg_log_foldl/2,
         msg_re/1,
         msg_re_orig/1,
	 msg_con/1,
         msg_text/0,
         nested_foldl/3,
	 stats/0,
         validate_dump_config/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec shortcode_msgs(Shortcode::string()) -> Info
%%  Info = list({Status,{Date,Time},Msg})
%% @doc Returns the unarchived (orig) msgs for the shortcode named Shortcode
%%  in reverse chronological order.
%% @end 
shortcode_msgs(Shortcode) ->
    F = fun () ->
                lists:reverse(
                  lists:keysort(2,
                                [ {Status,
                                   calendar:now_to_local_time(erms_uuid:nowtime(Id)),
                                   hd(mnesia:read({msg, Id}))}
                                  || {msg_status,Id,_,_,_,_,Status} <-
                                         mnesia:match_object({msg_status,'_','_','_',Shortcode,'_','_'})]))
        end,
    {atomic, List} = mnesia:transaction(F),
    List.

%% @spec shortcode_msgs(Shortcode,Limit) -> [{Status,{Date,Time},Msg}]
%% @doc Returns the first Limit unarchived (orig) msgs for the shortcode
%%  named Shortcode in reverse chronological order.
%% @end
shortcode_msgs(Shortcode, Limit) when is_integer(Limit) ->
    lists:sublist(shortcode_msgs(Shortcode), Limit);
shortcode_msgs(Shortcode, _) ->
    shortcode_msgs(Shortcode).

%% @spec reconfigure_shortcode(File) -> ok | Error
%% @doc Loads a single shortcode from the file named File and writes
%%  the definition to the database.
%%  You'll need to erms_shortcode:reconfigure(ShortcodeName) afterwards.
%% @end
reconfigure_shortcode(File) when is_list(File) ->
    {ok, [Shortcode]} = file:consult(File),
    reconfigure_shortcode(Shortcode);
reconfigure_shortcode(Shortcode) when is_record(Shortcode, shortcode) ->
    {atomic, ok} = mnesia:transaction(fun () -> mnesia:write(Shortcode) end),
    ok.

%% @spec inject(Connection::string(),
%%              Shortcode::string(),
%%              From::string(),
%%              To::string(),
%%              Text::string()) -> ok | error
%% @doc Injects an MT from connection Connection and shortcode Shortcode
%%  from number From, to number To with message body of Text.
%% @end
inject(Connection, Shortcode, From, To, Text) ->
    inject(Connection, Shortcode, From, To, Text, mt).

%% @spec inject(Connection::string(),
%%              Shortcode::string(),
%%              From::string(),
%%              To::string(),
%%              Text::string(),
%%              Direction) -> ok | error
%%   Direction = mt | mo
%% @doc Injects an mt or mo from connection Connection and shortcode
%%  Shortcode from number From, to number To with message body of Text.
%% @end
inject(Connection, Shortcode, From, To, Text, Direction) ->
    erms_msg_queue:queue(Connection, Shortcode,
                         erms_msg:msg(From,To,Text),
                         Direction).

%%====================================================================
%% Internal functions
%%====================================================================

delivery_times() ->
    delivery_times(#delivery_success{_='_'}).

delivery_times(Pat) when is_record(Pat, delivery_success); is_record(Pat, delivery_failure) ->
    Successes = mnesia:match_object(Pat),
    Times = lists:map(fun (D) -> delivery_time(D) end, Successes),
    STimes = lists:sort(Times),
    Sum = lists:sum(STimes),
    {{lists:min(STimes),
      lists:max(STimes),
      Sum,
      Sum / length(STimes)},
     STimes}.

%% @spec delivery_time(DeliveryRecord) -> Time::integer()
%%  DeliveryRecord = #delivery_success{} | #delivery_failure{}
%% @doc Returns the time (in milliseconds) between queueing and delivery in
%%  milliseonds.
%% @end
delivery_time(#delivery_success{time=T,orig_msg_id=U}) ->
    timer:now_diff(T,erms_uuid:nowtime(U)) / 1000;
delivery_time(#delivery_failure{time=T,orig_msg_id=U}) ->
    timer:now_diff(T,erms_uuid:nowtime(U)) / 1000.

connections() ->
    {atomic, Cs} = mnesia:transaction(fun () ->
                                              mnesia:match_object(#connection{_='_'})
                                      end),
    Cs.

connection(C) ->
    hd(mnesia:dirty_read({connection, C})).

shortcodes() ->
    {atomic, Ss} = mnesia:transaction(fun () ->
                                              mnesia:match_object(#shortcode{_='_'})
                                      end),
    Ss.

shortcode(S) ->
    hd(mnesia:dirty_read({shortcode, S})).

logins() ->
    {atomic, L} = mnesia:transaction(fun () ->
                                             mnesia:match_object(#login{_='_'})
                                     end),
    L.

msg(From, To, Text) ->
    erms_msg:msg(From, To, Text).

sync_deliver(Connection, Message) ->
    erms_connection:sync_deliver(Connection, Message, timer:seconds(10)).

trace(erms) ->
    fprof:trace([start, {procs, erms_sup:child_pids()}]);
trace(queue) ->
    fprof:trace([start, {procs, [erms_msg_queue]}]).

untrace() ->
    fprof:trace([stop]),
    fprof:profile(),
    fprof:analyse().

dbg() ->
    IO = group_leader(),
    dbg:tracer(node(), process,
               {fun (T, S) ->
                       io:format(IO, "Trace (~p): ~p~n",
                                 [calendar:local_time(), T]),
                       S
                end,
                []}).

p(X,Y,Z, Flags) ->
    dbg:p(c:pid(X,Y,Z), Flags).


%% @spec reload_code() -> [{Mod, Result}]
%%  Result = ok | {error, Reason::term()}
%% @doc Reloads all the modules listed in the erms.app file
%% @end
reload_code() ->
    {ok, Modules} = application:get_key(erms, modules),
    lists:map(fun (M) -> {M, c:nl(M)} end, Modules).

%% @spec source_of(Module::atom()) -> Source::string() 
%% @doc Returns the source code for the module Module from the running
%% system (will only work for loaded modules).
%% @end
source_of(Mod) ->
    {Mod, Binary, _Fn} = code:get_object_code(Mod),
    {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Binary,[abstract_code]),
    io_lib:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(AC))]).

%% @spec print_source_of(Module::atom()) -> Source::string() 
%% @doc Prints to stdout (to your current shell process - will work
%% from a remote shell) the source code for the module Module from the
%% running system (will only work for loaded modules).
%% @end
print_source_of(Mod) ->
    io:format(group_leader(), "~s", [source_of(Mod)]).

validate_dump_config(File) ->
    {ok, Terms} = file:consult(File),
    Connections = [C || C <- Terms,
                        is_record(C, connection)],
    Shortcodes = [S || S <- Terms,
                       is_record(S, shortcode)],
    Logins = [L || L <- Terms,
                   is_record(L, login)],
    [{logins,
      [{L#login.type, L#login.username,
        erms_auth:valid_login(L, 
                              [S#shortcode.name || S <- Shortcodes],
                              [C#connection.name || C <- Connections])}
       || L <- Logins]},
     {shortcodes,
      [{S#shortcode.name,
        erms_shortcode:validate(S, [C#connection.name || C <- Connections])}
       || S <- Shortcodes]},
     {connections,
      [{C#connection.name,
        erms_connection:validate(C, [S#shortcode.name || S <- Shortcodes])}
       || C <- Connections]}].

msg_log_foldl(Fn, File) when is_function(Fn,2) ->
    {ok, Cont} = wrap_log_reader:open(File),
    case msg_log_foldl(Fn, Cont, wrap_log_reader:chunk(Cont), []) of
        {ok, Workers} when is_list(Workers) -> lists:map(fun wait_result/1, Workers);
        {error, R, Workers} -> {error, R, lists:map(fun wait_result/1, Workers)}
    end.

nested_foldl(Fn, Acc0, List) ->
    lists:foldl(fun (SubL,Acc) -> lists:foldl(fun (I, SAcc) -> Fn(I, SAcc) end, Acc, SubL) end, Acc0, List).

wait_result({Pid, Ref}) ->
      receive
	  {'DOWN', Ref, _, _, normal} -> 
              receive {?MODULE, Pid, Result} -> Result end;
	  {'DOWN', Ref, _, _, Reason} -> exit(Reason)
      end.

msg_log_foldl(_Fn, OldCont, {error, Reason}, Acc) ->
    wrap_log_reader:close(OldCont),
    {error, Reason, Acc};
msg_log_foldl(_Fn, _OldCont, {Cont, eof}, Acc) ->
    wrap_log_reader:close(Cont),
    {ok, Acc};
msg_log_foldl(Fn, _OldCont, {Cont, Terms}, Acc) ->
    msg_log_foldl(Fn, Cont, wrap_log_reader:chunk(Cont),
                  process_terms(Fn, Terms,Acc));
msg_log_foldl(Fn, _OldCont, {Cont, Terms, BadBytes}, Acc) ->
    ?WARN("Missing ~p bytes in wraplog.", [BadBytes]),
    msg_log_foldl(Fn, Cont, wrap_log_reader:chunk(Cont),
                  process_terms(Fn, Terms,Acc)).

process_terms(Fn, Terms, Acc) ->
    Master = self(),
    Worker = erlang:spawn_monitor(fun () ->
                                          Master ! {?MODULE, self(), lists:foldl(Fn, [], Terms)}
                                  end),
    [Worker|Acc].

msg_re(RE) ->
    {ok, Re} = regexp:parse(RE),
    fun (#archived_msg{version=2,
                       msg_id=_Id,
                       archive_date=_Date,
                       msg_status=_Status,
                       msgs=Msgs,
                       delivery_details=_DeliveryInfo}, Acc) ->
            This = lists:foldl(fun (#msg{text=T}, MAcc) ->
                                       case regexp:first_match(T, Re) of
                                           {match, _, _} -> [T|MAcc];
                                           _ -> MAcc
                                       end
                               end, [], Msgs),
            lists:append(This, Acc);
        (_, Acc) -> Acc
    end.

msg_re_orig(RE) ->
    {ok, Re} = regexp:parse(RE),
    fun (#archived_msg{version=2,
                       msg_id=Id,
                       archive_date=_Date,
                       msg_status=_Status,
                       msgs=Msgs,
                       delivery_details=_DeliveryInfo}, Acc) ->
            M = erms_msg:fetch(Id, Msgs),
            case regexp:first_match(M#msg.text, Re) of
                {match, _, _} -> [{Id, M#msg.text}|Acc];
                _ -> Acc
            end
    end.

msg_con({connection, Dest}) when is_list(Dest) ->
    fun (#archived_msg{version=2,
                       msg_id=_Id,
                       archive_date=_Date,
                       msg_status=_Status,
                       msgs=_Msgs,
                       delivery_details=DeliveryInfo} = A, Acc) ->
	    case lists:any(fun (#delivery_success{destination=D})
			       when D =:= {connection, Dest} -> true;
			       (_) -> false
			   end,
			   DeliveryInfo) of
		true -> [A|Acc];
		false -> Acc
	    end
    end.

msg_text() ->
    fun (#archived_msg{version=2,
                       msg_id=Id,
                       archive_date=_Date,
                       msg_status=_Status,
                       msgs=Msgs,
                       delivery_details=_DeliveryInfo}, Acc) ->
            [T || #msg{id=I,text=T} <-Msgs,
                  I =:= Id] ++ Acc
    end.

stats() ->
    {erms_rpc:multicall(fun () -> 
				{node(), erms_stats:current_stats()}
			end),
     erms_stats:global_get_counters()}.
    
