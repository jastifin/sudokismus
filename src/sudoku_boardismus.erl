%% @author Juha Stalnacke <juha.stalnacke@gmail.com>
%% @copyright 2014 Juha Stalnacke
%% This file is part of Sudokismus.
%% 
%% Sudokismus is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% Sudokismus is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with Sudokismus.  If not, see <http://www.gnu.org/licenses/>.
%% @end
%% @doc Sudoku board

-module(sudoku_boardismus).

-include_lib("eunit/include/eunit.hrl").

-export([read_file/1,
         get_row/1,
         get_col/1,
         get_cell/1,
         get_value/3,
         extract_zones/1,
         extract_rows/1,
         extract_cols/1,
         get_row_ids/0,
         get_col_ids/0,
         get_cell_ids/0,
         get_other_rows/1,
         get_other_cols/1,
         get_other_cells/1,
         remove_from_col/3,
         remove_from_row/3,
         remove_and_set_values/2,
         remove_from_zone/3,
         set_values/2,
         set_lesser_values/3,
         belongs_to_cell/3,
         replace_value/2,
         replace_lesser_values/3,
         count_numbers_in_col/2,
         count_numbers_in_row/2,
         find_positions/1,
         find_same_numbers/2,
         find_same_positions/2,
         is_done/1]).

-type element_info() :: {Row    ::non_neg_integer(),
                         Col    ::non_neg_integer(),
                         Zone   ::non_neg_integer(),
                         Numbers::[non_neg_integer()]}.

-export_type([element_info/0]).

-spec read_file(Filename) -> [element_info()] | {error, term()} when
    Filename ::string().
read_file(Filename) ->
	case file:open(Filename,[read]) of
		{ok, Device} ->
			read_lines(Device);
		{error, Reason} ->
			{error, Reason}
	end.

read_lines(Device) ->
    read_line(Device, [], 1).

read_line(Device, Board, LineNmb) ->
	case file:read_line(Device) of
		{ok, Data} ->
			read_line(Device, process_line(Data, Board, LineNmb), LineNmb + 1);
		eof ->
			Board;
		{error, _} ->
			[]
	end.

process_line(Line, Board, LineNmb) ->
%    re:run("1:2;1:2,3,4;2:0\n","(([1-9]):([,0-9]*))(?:;|\n|)",[global,{capture, all_but_first, list}]).
    case re:run(Line, "(([1-9]):([,0-9]*))(?:;|\n|)", [global, {capture, all_but_first, list}]) of
        {match, Captured} ->
            ?assert(length(Captured) == 9),
            Board ++ lists:map(
              fun({ColNmb, [_, Id, NumbersString]}) ->
                      case NumbersString of
                          "0" ->
                              {LineNmb, ColNmb, list_to_integer(Id), lists:seq(1, 9)};
                          _   ->
                              Numbers = lists:map(
                                          fun(N) ->
                                                  list_to_integer(N)
                                          end, string:tokens(NumbersString, ",")),
                              {LineNmb, ColNmb, list_to_integer(Id), Numbers}
                      end
              end, lists:zip(lists:seq(1, 9), Captured));
        _                 ->
            erlang:error(bad_format)
    end.

process_line_test() ->
  ?assert( process_line("1:0;1:0;1:0;2:0;2:0;2:0;3:0;3:0;3:0\n", [], 1) =:= [
    {1, 1, 1, lists:seq(1, 9)}, {1, 2, 1, lists:seq(1, 9)}, {1, 3, 1, lists:seq(1, 9)},
    {1, 4, 2, lists:seq(1, 9)}, {1, 5, 2, lists:seq(1, 9)}, {1, 6, 2, lists:seq(1, 9)},
    {1, 7, 3, lists:seq(1, 9)}, {1, 8, 3, lists:seq(1, 9)}, {1, 9, 3, lists:seq(1, 9)}]),

  ?assert( process_line("1:1;1:2;1:3;2:1,2;2:0;2:0;3:0;3:0;3:0\n", [], 1) =:= [
    {1, 1, 1, [1]}, {1, 2, 1, [2]}, {1, 3, 1, [3]},
    {1, 4, 2, lists:seq(1, 2)}, {1, 5, 2, lists:seq(1, 9)}, {1, 6, 2, lists:seq(1, 9)},
    {1, 7, 3, lists:seq(1, 9)}, {1, 8, 3, lists:seq(1, 9)}, {1, 9, 3, lists:seq(1, 9)}]).

%% get_col(Values)

-spec get_col(Values) -> Col | no_return() when
    Values ::[element_info()],
    Col    ::non_neg_integer().

get_col(Values) ->
    Cols = extract_cols(Values),
    case length(Cols) of
        1 -> lists:nth(1, Cols);
        _ -> error(column_undetermined)
    end.

%% get_row(Values)

-spec get_row(Values) -> Row | no_return() when
    Values ::[element_info()],
    Row    ::non_neg_integer().

get_row(Values) ->
    Rows = extract_rows(Values),
    case length(Rows) of
        1 -> lists:nth(1, Rows);
        _ -> error(row_undetermined)
    end.

%% get_cell(Values)

-spec get_cell(Values) -> Zone | no_return() when
    Values ::[element_info()],
    Zone   ::non_neg_integer().

get_cell(Values) ->
    Zones = extract_zones(Values),
    case length(Zones) of
        1 -> lists:nth(1, Zones);
        _ -> error(zone_undetermined)
    end.

%% extract_rows(Values)
%% Return unique row indexes.

-spec extract_rows(Values) -> RowIndexes when
    Values     ::[element_info()],
    RowIndexes ::list(Index),
    Index      ::integer().

extract_rows(Values) ->
    extract_direction(Values,[], row).

%% extract_cols(Values)
%% Return unique column indexes.

-spec extract_cols(Values) -> ColIndexes when
    Values     ::[element_info()],
    ColIndexes ::list(Index),
    Index      ::integer().

extract_cols(Values) ->
    extract_direction(Values,[], col).

%% extract_zones(Values)

-spec extract_zones(Values) -> ZoneIndexes when
    Values      ::[element_info()],
    ZoneIndexes ::[non_neg_integer()].

extract_zones(Values) ->
    extract_direction(Values, [], zone).

extract_direction([], Positions, _) ->
    lists:reverse(Positions);
extract_direction([{Row, _, _, _} | Tail], Positions, row) ->
    case lists:member(Row, Positions) of
        true  -> extract_direction(Tail, Positions, row);
        false -> extract_direction(Tail, [Row | Positions], row)
    end;
extract_direction([{_, Col, _, _} | Tail], Positions, col) ->
    case lists:member(Col, Positions) of
        true  -> extract_direction(Tail, Positions, col);
        false -> extract_direction(Tail, [Col | Positions], col)
    end;
extract_direction([{_, _, Zone, _} | Tail], Positions, zone) ->
    case lists:member(Zone, Positions) of
        true  -> extract_direction(Tail, Positions, zone);
        false -> extract_direction(Tail, [Zone | Positions], zone)
    end.

%% get_row_ids()
%% list of row ids

-spec get_row_ids() -> [atom()].

get_row_ids() ->
    [list_to_atom("row" ++ integer_to_list(Id)) || Id <- lists:seq(1, 9)].

%% get_col_ids()
%% list of col ids

-spec get_col_ids() -> [atom()].

get_col_ids() ->
    [list_to_atom("col" ++ integer_to_list(Id)) || Id <- lists:seq(1, 9)].

%% get_cell_ids()
%% list of cell ids

-spec get_cell_ids() -> [atom()].

get_cell_ids() ->
    [list_to_atom("cell" ++ integer_to_list(Id)) || Id <- lists:seq(1, 9)].

%% get_other_rows(ThisRow)
%% list of row ids other than ThisRow

-spec get_other_rows(ThisRow) -> [atom()] when
    ThisRow ::atom().

get_other_rows(ThisRow) ->
    lists:filter(fun(Row) -> Row /= ThisRow end, get_row_ids()).

%% get_other_cols(ThisCol)
%% list of col ids other that ThisCol

-spec get_other_cols(ThisCol) -> [atom()] when
    ThisCol ::atom().

get_other_cols(ThisCol) ->
    lists:filter(fun(Col) -> Col /= ThisCol end, get_col_ids()).

%% get_other_cells(ThisCell)
%% list of cell ids other than ThisCell

-spec get_other_cells(ThisCell) -> [atom()] when
    ThisCell ::atom().

get_other_cells(ThisCell) ->
    lists:filter(fun(Cell) -> Cell /= ThisCell end, get_cell_ids()).

%% replace_value(Old, Replace)

-spec replace_value(Old, Replace) -> New when
    Old     ::element_info(),
    Replace ::[element_info()],
    New     ::element_info().

replace_value(Old, []) ->
    Old;
replace_value({OldR, OldC, _, _}, [{OldR, OldC, NewZone, NewValues} | _]) ->
    {OldR, OldC, NewZone, NewValues};
replace_value(Old, [_ | Tail]) ->
    replace_value(Old, Tail).

%% set_values(ReplaceList, OldList)

-spec set_values(ReplaceList, OldList) -> NewList when
    ReplaceList ::[element_info()],
    OldList     ::[element_info()],
    NewList     ::[element_info()].

set_values(ReplaceList, OldList) ->
    lists:reverse(
      lists:foldl(
        fun(Old, AccIn) ->
                [replace_value(Old, ReplaceList) | AccIn]
        end, [], OldList)).

%% replace_lesser_values(Old, Replace1, Replace2)

-spec replace_lesser_values(Old, Replace1, Replace2) -> New when
    Old      ::element_info(),
    Replace1 ::[element_info()],
    Replace2 ::[element_info()],
    New      ::element_info().

replace_lesser_values(Old, [], []) ->
    Old;
replace_lesser_values({OldR, OldC, _, _},
                      [{OldR, OldC, NewZone1, NewValues1} | _],
                      [{OldR, OldC, NewZone2, NewValues2} | _]) ->
    case length(NewValues1) < length(NewValues2) of
        true  -> {OldR, OldC, NewZone1, NewValues1};
        false -> {OldR, OldC, NewZone2, NewValues2}
    end;
replace_lesser_values(Old, [_ | Tail1], [_ | Tail2]) ->
    replace_lesser_values(Old, Tail1, Tail2).

set_lesser_values(ReplaceList1, ReplaceList2, OldList) ->
    lists:reverse(
      lists:foldl(
        fun(Old, AccIn) ->
                [replace_lesser_values(Old, ReplaceList1, ReplaceList2) | AccIn]
        end, [], OldList)).

%% remove_and_set_values(ReplaceList, OldList)
%% As set_values but remove all numbers from OldList by ReplaceList.
remove_and_set_values(ReplaceList, OldList) ->
    RemovedNumbers = sudoku_utilitus:remove_duplicates(
                       lists:foldl(
                         fun({Row, Col, _Zone, Numbers}, PreviouslyRemoved) ->
                                 case belongs_to_cell(Row, Col, OldList) of
                                     true  -> PreviouslyRemoved ++ Numbers;
                                     false -> PreviouslyRemoved
                                 end
                         end, [], ReplaceList)),
    lists:foldl(
      fun({Row, Col, Zone, Values}, AccIn) ->
              case belongs_to_cell(Row, Col, OldList) of
                  true  -> set_values([{Row, Col, Zone, Values}], AccIn);
                  false -> AccIn
              end
      end, remove_all(RemovedNumbers, OldList), ReplaceList).

remove_from(Pos, N, List, Direction) when is_integer(N)->
    remove_from(Pos, [N], List, Direction);
remove_from(Pos, Numbers, List, Direction) when is_list(Numbers) ->
    Index = get_index(Direction),
    lists:map(
      fun(Tuple) ->
              case element(Index, Tuple) == Pos of
                  true  -> {element(1, Tuple), element(2, Tuple), element(3, Tuple), element(4, Tuple) -- Numbers};
                  false -> Tuple
              end
      end, List).

%% remove_from_row(Row, Numbers, List)
%% Remove numbers Numbers from row Row in the list List.

-spec remove_from_row(Row, Numbers, List) -> List2 when
    Row     ::integer(),
    Numbers ::list(integer()),
    List    ::[element_info()],
    List2   ::[element_info()].

remove_from_row(Row, Numbers, List) ->
    remove_from(Row, Numbers, List, row).

%% remove_from_col(Col, Numbers, List)
%% Remove number Numbers from col Col in the list List.

-spec remove_from_col(Col, Numbers, List) -> List2 when
    Col     ::integer(),
    Numbers ::list(integer()),
    List    ::[element_info()],
    List2   ::[element_info()].

remove_from_col(Col, Numbers, List) ->
    remove_from(Col, Numbers, List, col).

%% remove_all(Numbers, Values)

-spec remove_all(Numbers, Values) -> Values2 when
    Numbers ::[non_neg_integer()],
    Values  ::[element_info()],
    Values2 ::[element_info()].

remove_all(Numbers, Values) ->
    lists:map(fun({R, C, Z, List}) -> {R, C, Z, List -- Numbers} end, Values).

%% remove_from_zone(Zone, Numbers, Values)

-spec remove_from_zone(Zone, Numbers, Values) -> Values2 when
    Zone    ::non_neg_integer(),
    Numbers ::[non_neg_integer()],
    Values  ::[element_info()],
    Values2 ::[element_info()].

remove_from_zone(Zone, Numbers, Values) ->
    lists:map(
      fun({R, C, Z, Nlist}) ->
              case Z == Zone of
                  true  -> {R, C, Z, Nlist -- Numbers};
                  false -> {R, C, Z, Nlist}
              end
      end, Values).
%% belongs_to_cell(Row, Col, Values)

-spec belongs_to_cell(Row, Col, Values) -> boolean() when
    Row    ::integer(),
    Col    ::integer(),
    Values ::[element_info()].

belongs_to_cell(_, _, []) ->
    false;
belongs_to_cell(Row, Col, [{Row, Col, _, _} | _]) ->
    true;
belongs_to_cell(Row, Col, [_ | Tail]) ->
    belongs_to_cell(Row, Col, Tail).

%% get_index(Direction)
%% generate_msgs(Values, CellType, CellId)

-spec get_index(Direction) -> integer() when
          Direction ::zone | row | col.

get_index(row) ->
    1;
get_index(col) ->
    2;
get_index(zone) ->
    3.

% get_value(Row, Col, Values)

-spec get_value(Row, Col, Values) -> element_info() when
    Row    ::non_neg_integer(),
    Col    ::non_neg_integer(),
    Values ::[element_info()].

get_value(_, _, []) ->
    [];
get_value(Row, Col, [{Row, Col, Z, Numbers} | _]) ->
    {Row, Col, Z, Numbers};
get_value(Row, Col, [_ | Tail]) ->
    get_value(Row, Col, Tail).

count_numbers_in_direction([], Counts) ->
    Counts;
count_numbers_in_direction([{_, _, _, Numbers} | Tail], Counts) ->
    count_numbers_in_direction(Tail, sudoku_utilitus:increment_counts(Counts, Numbers)).

%% count_numbers_in_row(Values, Row)
%% Counts how many times each number from 1-9 occur in a row

-spec count_numbers_in_row(Values, Row) -> List when
    Values ::[sudoku_boardismus:element_info()],
    List   ::[Count],
    Row    ::integer(),
    Count  ::integer().

count_numbers_in_row(Values, Row) ->
    Initial = lists:duplicate(9, 0),
    count_numbers_in_direction(
      lists:filter(
        fun({R, _, _, _}) ->
                R == Row
        end, Values), Initial).

%% count_numbers_in_col(Values)
%% Counts how many times each number 1-9 occurs in a column

-spec count_numbers_in_col(Values, Col) -> List when
    Values  ::[sudoku_boardismus:element_info()],
    Col     ::non_neg_integer(),
    List    ::[Count],
    Count   ::integer().

count_numbers_in_col(Values, Col) ->
    Initial = lists:duplicate(9, 0),
    count_numbers_in_direction(
      lists:filter(
        fun({_, C, _, _}) ->
                C == Col
        end, Values), Initial).

%% find_positions(Values)

-spec find_positions(Values) -> [[{Row, Col}]] when
    Values ::[sudoku_boardismus:element_info()],
    Row    ::non_neg_integer(),
    Col    ::non_neg_integer().

find_positions(Values) ->
    lists:map(
      fun(N) ->
              lists:filtermap(
                fun({R, C, _, Numbers}) ->
                        case lists:member(N, Numbers) of
                            true  -> {true, {R, C}};
                            false -> false
                        end
                end, Values)
      end, lists:seq(1, 9)).

%% find_same_numbers(Numbers, Values)

-spec find_same_numbers(Numbers, Values) -> [sudoku_boardismus:element_info()] when
    Numbers ::[non_neg_integer()],
    Values  ::[sudoku_boardismus:element_info()].

find_same_numbers(Numbers, Values) ->
    find_same_elements(Numbers, Values, 4).

%% find_same_positions(Positions, NPositions)
-spec find_same_positions(Positions, NPositions) -> [{N1,[{Row, Col}]}] when
    Positions  ::[{non_neg_integer(), non_neg_integer()}],
    NPositions ::[{N2,[{non_neg_integer(), non_neg_integer()}]}],
    N1         ::non_neg_integer(),
    Row        ::non_neg_integer(),
    Col        ::non_neg_integer(),
    N2         ::non_neg_integer().

find_same_positions(Positions, NPositions) ->
    find_same_elements(Positions, NPositions, 2).

find_same_elements(Elements,Haystack,Idx) ->
    lists:filter(
      fun(Tuple) ->
              length(Elements) == length(element(Idx, Tuple)) andalso lists:all(
                fun(E) ->
                        lists:member(E, Elements)
                end, element(Idx, Tuple))
      end, Haystack).

%% is_done(Values)

-spec is_done(Values) -> boolean() when
    Values ::[element_info()].

is_done(Values) ->
    lists:all(
      fun({_, _, _, Numbers}) ->
              length(Numbers) == 1
      end, Values).
