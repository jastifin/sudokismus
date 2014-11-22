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
%% @doc Unit tests for sudoku_boardismus.

-module(sudoku_boardismus_test).

-include_lib("eunit/include/eunit.hrl").

get_cell_test() ->
    ?assertEqual(sudoku_boardismus:get_cell([{2, 1, 4, [5, 6]}]), 4).

get_row_test() ->
    ?assertEqual(sudoku_boardismus:get_row([{2, 1, 4, [5, 6]}]), 2).

get_col_test() ->
    ?assertEqual(sudoku_boardismus:get_col([{2, 1, 4, [5, 6]}]), 1).


belongs_to_cell_test() ->
    ?assertEqual(sudoku_boardismus:belongs_to_cell(1, 1, []), false),
    ?assertEqual(sudoku_boardismus:belongs_to_cell(1, 1, [{1, 1, 2, [1]}]), true),
    ?assertEqual(sudoku_boardismus:belongs_to_cell(1, 2, [{1, 1, 2, [2]}]), false).

remove_from_zone_test() ->
    Values1   = [{1, C, 4, lists:seq(1, 9)       } || C <- lists:seq(1, 3) ] ++ [{1, C, 5, lists:seq(1, 9)} || C <- lists:seq(4, 9) ],
    Expected1 = [{1, C, 4, lists:seq(1, 9) -- [3]} || C <- lists:seq(1, 3) ] ++ [{1, C, 5, lists:seq(1, 9)} || C <- lists:seq(4, 9) ],
    ?assertEqual(sudoku_boardismus:remove_from_zone(4, [3], Values1), Expected1),

    Values2   = [{R, 3, 5, lists:seq(1, 9)         } || R <- lists:seq(1, 3) ] ++ [{R, 1, 3, lists:seq(1, 9)} || R <- lists:seq(4, 9) ],
    Expected2 = [{R, 3, 5, lists:seq(1, 9) -- [1, 3]} || R <- lists:seq(1, 3) ] ++ [{R, 1, 3, lists:seq(1, 9)} || R <- lists:seq(4, 9) ],
    ?assertEqual(sudoku_boardismus:remove_from_zone(5, [1, 3], Values2), Expected2),

    Values3   = [{1, C, 4, lists:seq(1, 9)} || C <- lists:seq(1, 3) ] ++ [{1, C, 5, lists:seq(1, 9)} || C <- lists:seq(4, 9) ],
    Expected3 = Values3,
    ?assertEqual(sudoku_boardismus:remove_from_zone(6, [3], Values3), Expected3).

extract_rows_test() ->
    ?assertEqual(sudoku_boardismus:extract_rows([]), []),
    ?assertEqual(sudoku_boardismus:extract_rows([{1, 1, 1, [2, 3]}]), [1]),
    ?assertEqual(sudoku_boardismus:extract_rows([{1, 1, 1, [2, 3]}, {1, 2, 1, [2, 3]}]), [1]),
    ?assertEqual(sudoku_boardismus:extract_rows([{1, 1, 1, [2, 3]}, {2, 2, 1, [2, 3]}]), [1, 2]).

extract_cols_test() ->
    ?assertEqual(sudoku_boardismus:extract_cols([]), []),
    ?assertEqual(sudoku_boardismus:extract_cols([{1, 1, 1, [2, 3]}]), [1]),
    ?assertEqual(sudoku_boardismus:extract_cols([{1, 1, 1, [2, 3]}, {1, 2, 1, [2, 3]}]), [1, 2]),
    ?assertEqual(sudoku_boardismus:extract_cols([{1, 1, 1, [2, 3]}, {2, 1, 1, [2, 3]}]), [1]).

extract_zones_test() ->
    ?assertEqual(sudoku_boardismus:extract_zones([]), []),
    ?assertEqual(sudoku_boardismus:extract_zones([{1, 1, 1, [2, 3]}]), [1]),
    ?assertEqual(sudoku_boardismus:extract_zones([{1, 1, 1, [2, 3]}, {1, 2, 2, [2, 3]}]), [1, 2]),
    ?assertEqual(sudoku_boardismus:extract_zones([{1, 1, 1, [2, 3]}, {2, 1, 1, [2, 3]}]), [1]).

replace_value_test() ->
    ?assertEqual(sudoku_boardismus:replace_value({1, 1, 1, [1, 2]}, [{1, 1, 1, [2]}]), {1, 1, 1, [2]}),
    ?assertEqual(sudoku_boardismus:replace_value({1, 1, 1, [1, 2]}, [{1, 1, 1, [2]}, {2, 1, 1, [2]}]), {1, 1, 1, [2]}),
    ?assertEqual(sudoku_boardismus:replace_value({1, 2, 1, [1, 2]}, [{1, 1, 1, [2]}]), {1, 2, 1, [1, 2]}).

set_values_test() ->
    ?assertEqual(sudoku_boardismus:set_values([], []), []),
    ?assertEqual(sudoku_boardismus:set_values([{1, 1, 2, [2]}], [{1, 1, 2, [1]}]), [{1, 1, 2, [2]}]),
    ?assertEqual(sudoku_boardismus:set_values([{1, 1, 2, [2]}], [{1, 2, 2, [1]}]), [{1, 2, 2, [1]}]),
    ?assertEqual(sudoku_boardismus:set_values([{1, 1, 2, [2]}], [{1, 1, 2, [1, 4]}, {1, 2, 2, [1]}, {2, 2, 2, [2, 3]}]), [{1, 1, 2, [2]}, {1, 2, 2, [1]}, {2, 2, 2, [2, 3]}]),
    ?assertEqual(sudoku_boardismus:set_values([{1, 1, 2, [2]}, {1, 4, 2, [4, 5]}], [{1, 3, 2, [1]}, {1, 1, 2, [1]}, {1, 4, 2, [4, 5, 6]}]), [{1, 3, 2, [1]}, {1, 1, 2, [2]}, {1, 4, 2, [4, 5]}]).

replace_lesser_values_test() ->
    ?assertEqual(sudoku_boardismus:replace_lesser_values({1, 1, 1, [1, 2, 3]}, [{1, 1, 1, [1, 2]}], [{1, 1, 1, [2]}]), {1, 1, 1, [2]}),
    ?assertEqual(sudoku_boardismus:replace_lesser_values({1, 1, 1, [1, 2]}, [{1, 1, 1, [2]}, {2, 1, 1, [2]}], [{1, 1, 1, [1, 2]}]), {1, 1, 1, [2]}),
    ?assertEqual(sudoku_boardismus:replace_lesser_values({1, 2, 1, [1, 2]}, [{1, 1, 1, [2]}], [{1, 1, 1, [2]}]), {1, 2, 1, [1, 2]}).

remove_and_set_values_test() ->
    ?assertEqual(sudoku_boardismus:remove_and_set_values([], []), []),
    ?assertEqual(sudoku_boardismus:remove_and_set_values([{1, 1, 2, [2]}], [{1, 1, 2, [1]}]), [{1, 1, 2, [2]}]),
    ?assertEqual(sudoku_boardismus:remove_and_set_values([{1, 1, 2, [2]}], [{1, 2, 2, [1]}, {2, 2, 2, [2, 3]}]), [{1, 2, 2, [1]}, {2, 2, 2, [2, 3]}]),
    ?assertEqual(sudoku_boardismus:remove_and_set_values([{1, 1, 2, [2]}], [{1, 1, 2, [1, 4]}, {1, 2, 2, [1]}, {2, 2, 2, [2, 3]}]), [{1, 1, 2, [2]}, {1, 2, 2, [1]}, {2, 2, 2, [3]}]),
    ?assertEqual(sudoku_boardismus:remove_and_set_values([{1, 1, 2, [2]}, {1, 4, 2, [4, 5]}], [{1, 3, 2, [1]}, {1, 1, 2, [1]}, {1, 4, 2, [4, 5, 6]}]), [{1, 3, 2, [1]}, {1, 1, 2, [2]}, {1, 4, 2, [4, 5]}]),

    ?assertEqual(sudoku_boardismus:remove_and_set_values(
              [{1, 3, 2, [4, 5]}, {1, 4, 2, [4, 5]}],
              [{1, 1, 2, [1]}, {1, 2, 2, [3]}, {1, 3, 2, [4, 5]}, {1, 4, 2, [4, 5]}]),
                 [{1, 1, 2, [1]}, {1, 2, 2, [3]}, {1, 3, 2, [4, 5]}, {1, 4, 2, [4, 5]}]),
    Values1 = [{1, 5, 2, [3, 6]},
               {2, 5, 2, [2]},
               {3, 5, 2, [3, 6]},
               {4, 5, 5, [1]},
               {5, 5, 5, [9]},
               {6, 5, 5, [7]},
               {7, 5, 8, [8]},
               {8, 5, 8, [5]},
               {9, 5, 8, [5]}],
    Expected1 = [{1, 5, 2, [3, 6]},
               {2, 5, 2, [2]},
               {3, 5, 2, [3, 6]},
               {4, 5, 5, [1]},
               {5, 5, 5, [9]},
               {6, 5, 5, [7]},
               {7, 5, 8, [8]},
               {8, 5, 8, [5]},
               {9, 5, 8, [5]}],
    ?assertEqual(sudoku_boardismus:remove_and_set_values([{1, 5, 2, [3, 6]}, {3, 5, 2, [3, 6]}], Values1), Expected1).

remove_from_row_test() ->
    ?assertEqual(sudoku_boardismus:remove_from_row(1, [1], []), []),
    ?assertEqual(sudoku_boardismus:remove_from_row(1, [1], [{1, 1, 2, [1]}]), [{1, 1, 2, []}]),
    ?assertEqual(sudoku_boardismus:remove_from_row(1, [1], [{1, 1, 2, [1, 2]}]), [{1, 1, 2, [2]}]),
    ?assertEqual(sudoku_boardismus:remove_from_row(1, [1], [{1, 1, 2, [1, 2]}, {1, 1, 2, [1, 3]}]), [{1, 1, 2, [2]}, {1, 1, 2, [3]}]),
    ?assertEqual(sudoku_boardismus:remove_from_row(1, [1], [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 3]}]), [{1, 1, 2, [2]}, {2, 1, 2, [1, 3]}]).

remove_from_col_test() ->
    ?assertEqual(sudoku_boardismus:remove_from_col(1, [1], []), []),
    ?assertEqual(sudoku_boardismus:remove_from_col(1, [1], [{1, 1, 2, [1]}]), [{1, 1, 2, []}]),
    ?assertEqual(sudoku_boardismus:remove_from_col(1, [1], [{1, 1, 2, [1, 2]}]), [{1, 1, 2, [2]}]),
    ?assertEqual(sudoku_boardismus:remove_from_col(1, [1], [{1, 1, 2, [1, 2]}, {1, 1, 2, [1, 3]}]), [{1, 1, 2, [2]}, {1, 1, 2, [3]}]),
    ?assertEqual(sudoku_boardismus:remove_from_col(2, [1], [{1, 2, 2, [1, 2]}, {1, 1, 2, [1, 3]}]), [{1, 2, 2, [2]}, {1, 1, 2, [1, 3]}]),
    ?assertEqual(sudoku_boardismus:remove_from_col(2, [1], [{1, 1, 2, [1, 3]}, {1, 2, 2, [1, 2]}]), [{1, 1, 2, [1, 3]}, {1, 2, 2, [2]}]),
    ?assertEqual(sudoku_boardismus:remove_from_col(1, [1], [{1, 1, 2, [1, 2]}, {2, 2, 2, [1, 3]}]), [{1, 1, 2, [2]}, {2, 2, 2, [1, 3]}]).

count_numbers_in_col_test() ->
    ?assertEqual(sudoku_boardismus:count_numbers_in_col([], 1), lists:duplicate(9, 0)),
    ?assertEqual(sudoku_boardismus:count_numbers_in_col([{1, 2, 2, [2]}], 1), lists:duplicate(9, 0)),
    ?assertEqual(sudoku_boardismus:count_numbers_in_col([{1, 2, 2, [2]}], 2), [0, 1, 0, 0, 0, 0, 0, 0, 0]),
    ?assertEqual(sudoku_boardismus:count_numbers_in_col([{1, 2, 2, [2]}, {1, 3, 2, [2, 4]}], 2), [0, 1, 0, 0, 0, 0, 0, 0, 0]).

count_numbers_in_row_test() ->
    ?assertEqual(sudoku_boardismus:count_numbers_in_row([], 1), lists:duplicate(9, 0)),
    ?assertEqual(sudoku_boardismus:count_numbers_in_row([{1, 2, 2, [2]}], 1), [0, 1, 0, 0, 0, 0, 0, 0, 0]),
    ?assertEqual(sudoku_boardismus:count_numbers_in_row([{1, 2, 2, [2]}, {1, 3, 2, [2, 4]}], 1), [0, 2, 0, 1, 0, 0, 0, 0, 0]).


find_positions_test() ->
    ?assertEqual(sudoku_boardismus:find_positions([]),
                 lists:duplicate(9, [])),
    ?assertEqual(sudoku_boardismus:find_positions([{1, 2, 3, [1]}]),
                 [[{1, 2}]] ++ lists:duplicate(8, [])),
    ?assertEqual(sudoku_boardismus:find_positions([{1, 2, 3, [1, 2]}]),
                 [[{1, 2}], [{1, 2}]] ++ lists:duplicate(7, [])),
    ?assertEqual(sudoku_boardismus:find_positions([{1, 2, 3, [1, 2]}, {1, 3, 3, [1]}]),
                [[{1, 2}, {1, 3}], [{1, 2}]] ++ lists:duplicate(7, [])).

find_same_positions_test() ->
    ?assertEqual(sudoku_boardismus:find_same_positions([{1, 2}], [{3, [{2, 3}]}, {4, [{1, 2}]}]), [{4, [{1, 2}]}]).

find_same_numbers_test() ->
    ?assertEqual(sudoku_boardismus:find_same_numbers([1], []), []),
    ?assertEqual(sudoku_boardismus:find_same_numbers([1], [{1, 2, 3, [2]}]), []),
    ?assertEqual(sudoku_boardismus:find_same_numbers([1], [{1, 2, 3, [1]}]), [{1, 2, 3, [1]}]),
    ?assertEqual(sudoku_boardismus:find_same_numbers([1], [{1, 2, 3, [1, 2]}]), []),
    ?assertEqual(sudoku_boardismus:find_same_numbers([1, 2], [{1, 2, 3, [1, 2]}]), [{1, 2, 3, [1, 2]}]).
