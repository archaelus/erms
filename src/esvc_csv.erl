%%% @copyright Catalyst IT Ltd (http://catalyst.net.nz)
%%%

-module(esvc_csv).

-export([read/1]).

-define(ASCII_TAB,9).
-define(ASCII_LF,10).
-define(ASCII_CR,13).
-define(ASCII_SP,32).
-define(ASCII_QUOTES,34).

%% @spec read(String) -> [Strings]
%% @doc Takes a CSV line and splits it into individual items,
%%      allowing for the use of quotes and cr/lf.
%%
%%      From trapexit.org "Comma Separated Values"
%%
%% @end

read(String) -> read(String, []).

%%%

read([], Acc) -> 
    lists:reverse(Acc);
read(String, []) -> 
    {Line, Rest} = read_line(String),
    read(Rest, [Line]);
read([?ASCII_LF|String], Acc) ->
    {Line, Rest} = read_line(String),
    read(Rest, [Line|Acc]);
read([?ASCII_CR,?ASCII_LF|String], Acc) ->
    {Line, Rest} = read_line(String),
    read(Rest, [Line|Acc]).
   
add_spaces(0, String) -> String;
add_spaces(Count, String) -> add_spaces(Count-1, [$ |String]).

read_item([?ASCII_QUOTES|T]) -> read_item_quoted(T, []);
read_item(Other) -> read_item(Other, 0, []).

read_item([?ASCII_SP|T], 0, []) -> read_item(T, 0, []);
read_item([?ASCII_TAB|T], 0, []) -> read_item(T, 0, []);
read_item([?ASCII_LF|T], _SpaceCount, Acc) -> {lists:reverse(Acc), [?ASCII_LF|T]};
read_item([?ASCII_CR,?ASCII_LF|T], _SpaceCount, Acc) -> {lists:reverse(Acc), [?ASCII_CR,?ASCII_LF|T]};
read_item([$,|T], _SpaceCount, Acc) -> {lists:reverse(Acc), [$,|T]};
read_item([], _SpaceCount, Acc) -> {lists:reverse(Acc), []};
read_item([?ASCII_TAB|T], SpaceCount, Acc) -> read_item(T, SpaceCount+1, Acc);
read_item([?ASCII_SP|T], SpaceCount, Acc) -> read_item(T, SpaceCount+1, Acc);
read_item([C|T], SpaceCount, Acc) -> read_item(T, 0, [C|add_spaces(SpaceCount, Acc)]).

read_item_quoted([?ASCII_QUOTES,?ASCII_QUOTES|T], Acc) -> read_item_quoted(T, [?ASCII_QUOTES|Acc]);
read_item_quoted([?ASCII_QUOTES|T], Acc) -> {lists:reverse(Acc), T};
read_item_quoted([C|T], Acc) -> read_item_quoted(T, [C|Acc]).

read_line(String) -> read_line(String,[]).

read_line([?ASCII_LF|T], Acc) -> {lists:reverse(Acc), [?ASCII_LF|T]};
read_line([?ASCII_CR,?ASCII_LF|T], Acc) -> {lists:reverse(Acc), [?ASCII_CR|T]};
read_line([], Acc) -> {lists:reverse(Acc), []};
read_line(String, []) -> {Item, Rest} = read_item(String), read_line(Rest, [Item]);
read_line([$,|String], Acc) -> {Item, Rest} = read_item(String), read_line(Rest, [Item|Acc]).
