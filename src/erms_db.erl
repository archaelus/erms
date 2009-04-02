%%%-------------------------------------------------------------------
%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Mnesia DB utility module
%% @end
%%%-------------------------------------------------------------------
-module(erms_db).
-include_lib("mnesia_model.hrl").

%% API
-export([recreate_db/0, recreate_db/1, dump/0,
         backup/0, backup/1, default_backup_name/0,
         change_node_name/4, change_node_name/5,
         drop_tables/0, init_node/0, init_node/1,
         restore/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

%% @spec init_node() -> ok
%% @see recreate_db/0
%% @doc Initalises a new ERMS node by creating an mnesia schema (with
%% disk copies on the local node) and then recreating the erms
%% database from priv/text/db.erl
%% @end
init_node() ->
    mnesia:create_schema([node()]),
    catch (mnesia:start()),
    recreate_db().

%% @spec init_node(Filename::string()) -> ok
%% @see recreate_db/1
%% @doc Initialises a new ERMS node from the mnesia text dump in
%% Filename. Uses recreate_db[{file, Filename}].
%% @end
init_node(File) ->
    mnesia:create_schema([node()]),
    catch (mnesia:start()),
    recreate_db([{file, File}]).    

%% @spec recreate_db() -> ok
%% @doc Recreates the ERMS db with the standard options. This should
%% enter the data in priv/text/db.erl into database tables (creating
%% such if necessary).
%% @end
recreate_db() ->
    recreate_db([]).

%% @spec recreate_db(Options::list(Option)) -> ok
%%   Option = drop | {drop, boolean()} | {file, Filename}
%% @doc Recreates the ERMS database given the list of Options. The
%% drop ({drop, true}) option drops existing ERMS tables before
%% recreating and reentering data. The {file, Filename} option
%% specifies the mnesia text dumpfile to use when creating the
%% database and entering data. The default dumpfile is
%% "priv/text/db.erl".
%% @end
recreate_db(Options) ->
    case proplists:get_value(drop, Options, false) of
        true -> drop_tables();
        false -> ok
    end,
    FileName = proplists:get_value(file, Options, "priv/text/db.erl"),
    mnesia:load_textfile(FileName).

%% @spec drop_tables() -> ok
%% @doc Deletes all mnesia tables.
%% @todo Fix this so that it only deletes ERMS tables.
%% @end
drop_tables() ->
    lists:foreach(fun (schema) ->
                          ok;
                      (Table) -> mnesia:delete_table(Table)
                  end,
                  mnesia:system_info(tables)).

%% @spec dump() -> ok
%% @doc Dumps the entire mnesia database to a dated text file in the
%% priv/text directory.
%% @end
dump() ->
    dump_to_textfile("priv/text/db-" ++
                     iso_8601_fmt(calendar:local_time())
                     ++ ".erl",
                    [{skip, ?ERMS_HIGH_VOLUME_TABLES}]).

change_node_name(From, To, Source, Target) ->
    change_node_name(mnesia_backup, From, To, Source, Target).

change_node_name(Mod, From, To, Source, Target) ->
    Switch =
        fun(Node) when Node == From -> To;
           (Node) when Node == To -> throw({error, already_exists});
           (Node) -> Node
        end,
    Convert =
        fun({schema, db_nodes, Nodes}, Acc) ->
                {[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
           ({schema, version, Version}, Acc) ->
                {[{schema, version, Version}], Acc};
           ({schema, cookie, Cookie}, Acc) ->
                {[{schema, cookie, Cookie}], Acc};
           ({schema, Tab, CreateList}, Acc) ->
                Keys = [ram_copies, disc_copies, disc_only_copies],
                OptSwitch =
                    fun({Key, Val}) ->
                            case lists:member(Key, Keys) of
                                true -> {Key, lists:map(Switch, Val)};
                                false-> {Key, Val}
                            end
                    end,
                {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc};
           (Other, Acc) ->
                {[Other], Acc}
        end,
    mnesia:traverse_backup(Source, Mod, Target, Mod, Convert, switched).


%% @spec backup(Filename::string()) -> ok
%% @doc Writes an mnesia backup to the file Filename.
%% @end
backup(Name) ->
    mnesia:backup(Name).

%% @spec backup() -> ok
%% @doc Writes an mnesia backup to a dated file in priv/backups.
%% @end
backup() ->
    backup(default_backup_name()).

default_backup_name() ->
    "priv/backups/erms-db-" ++ iso_8601_fmt(calendar:local_time()).

%% @spec restore(Filename::string()) -> ok
%% @doc Restores the mnesia database from the named backup file.
%% @end
restore(Name) ->
    mnesia:restore(Name, []).

%%====================================================================
%% Internal functions
%%====================================================================

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B_~2.10.0B:~2.10.0B:~2.10.0B",
                  [Year, Month, Day, Hour, Min, Sec]).

dump_to_textfile(File, Options) ->
    dump_to_textfile(mnesia_lib:is_running(), file:open(File, [write]), Options).
dump_to_textfile(yes, {ok, F}, Options) ->
    SkipTables = proplists:get_value(skip, Options, []),
    Tabs = lists:foldl(fun lists:delete/2, mnesia_lib:local_active_tables(), [schema|SkipTables]),
    Defs = lists:map(fun(T) -> {T, [{record_name, mnesia_lib:val({T, record_name})},
				    {attributes, mnesia_lib:val({T, attributes})}]} 
		     end,
		     Tabs),
    io:format(F, "~p.~n", [{tables, Defs}]),
    lists:foreach(fun(T) -> dump_tab(F, T) end, Tabs),
    file:close(F);
dump_to_textfile(_,_,_) -> error.

    
dump_tab(F, T) ->
    W = mnesia_lib:val({T, wild_pattern}),
    {atomic,All} = mnesia:transaction(fun() -> mnesia:match_object(T, W, read) end),
    lists:foreach(fun(Term) -> io:format(F,"~p.~n", [setelement(1, Term, T)]) end, All).
