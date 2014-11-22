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
%% @doc unit tests for sudoku_messagismus.

-module(sudoku_messagismus_test).

-include_lib("eunit/include/eunit.hrl").

generate_msgs_test() ->
    Values   = [{1, 1, 2, [1]} | [ {1, C, 2, lists:seq(2, 9)} || C <- lists:seq(2, 8)]],
    Msgs     = sudoku_messagismus:generate_msgs(Values, row, cell1),
    Expected = [{row, cell1, has, {1, 1, 2, [1]}}],
    ?assertEqual(Msgs, Expected),

    Values2   = [ {1, C, 2, lists:seq(1, 9)} || C <- lists:seq(1, 8)],
    Msgs2     = sudoku_messagismus:generate_msgs(Values2, row, cell1),
    Expected2 = [],
    ?assertEqual(Msgs2, Expected2),

    Values3   = [{1, 1, 2, [1]} | [ {R, 1, 2, lists:seq(2, 9)} || R <- lists:seq(2, 8)]],
    Msgs3     = sudoku_messagismus:generate_msgs(Values3, cell, cell2),
    Expected3 = [{cell, cell2, has, {1, 1, 2, [1]}}] ++ [{cell, 2, has_in_col, 1, N} || N <- lists:reverse(lists:seq(2, 9))],
    ?assertEqual(Msgs3, Expected3),

    Values4   = [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 2]}] ++ [ {R, 1, 2, lists:seq(2, 9)} || R <- lists:seq(3, 9)],
    Msgs4     = sudoku_messagismus:generate_msgs(Values4, cell, cell2),
    Expected4 = [{cell, 2, has_in_col, 1, N} || N <- lists:reverse(lists:seq(1, 9))] ++
                    [{cell, cell2, has_set, 2, [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 2]}]}],
    ?assertEqual(Msgs4, Expected4),

    Values5   = [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 2]}, {3, 1, 2, [3]}] ++ [ {R, 1, 2, lists:seq(4, 9)} || R <- lists:seq(4, 9)],
    Msgs5     = sudoku_messagismus:generate_msgs(Values5, cell, cell2),
    Expected5 = [{cell, cell2, has, {3, 1, 2, [3]}}] ++
                    lists:reverse([{cell, 2, has_in_col, 1, N} || N <- lists:seq(1, 2)] ++
                                      [{cell, 2, has_in_col, 1, N} || N <- lists:seq(4, 9)]) ++
                    [{cell, cell2, has_set, 2, [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 2]}]}],

    ?assertEqual(Msgs5, Expected5),

    Values6   = [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 2]}] ++ [ {R, 1, 2, lists:seq(2, 9)} || R <- lists:seq(3, 9)],
    Msgs6     = sudoku_messagismus:generate_msgs(Values6, col, col2),
    Expected6 = [{col, col2, has_set, 2, [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 2]}]}],
%%     Expected6 = [{col, col2, xwing_possibility, [{1, 1, 2, [1]}, {2, 1, 2, [1]}]}] ++
%%                     [{col, col2, has_set, 2, [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 2]}]}],
    ?assertEqual(Msgs6, Expected6).

generate_cell_msgs_test() ->
    ?assertEqual(sudoku_messagismus:generate_cell_msgs([]), []),
%%    io:format(user, "~p~n", [generate_cell_msgs([{1, 1, 1, [1]}, {1, 2, 1, [2]}], cell1, [1], [1, 2])]),
    ?assertEqual(sudoku_messagismus:generate_cell_msgs([{1, 1, 1, [1]}, {1, 2, 1, [2]}]), []),
    ?assertEqual(sudoku_messagismus:generate_cell_msgs([{1, 1, 1, [1, 2]}, {1, 2, 1, [2]}]), [{cell, 1, has_in_row, 1, 2}]),
    ?assertEqual(sudoku_messagismus:generate_cell_msgs([{1, 1, 1, [1, 2]}, {1, 2, 1, [2]}, {2, 1, 1, [1]}]),
                [{cell, 1, has_in_row, 1, 2}, {cell, 1, has_in_col, 1, 1}]).

get_xwing_set_messages_test() ->
    ?assertEqual(sudoku_messagismus:get_xwing_set_messages(
                   [{2, 2, 1, [2, 3]}, {2, 3, 1, [1, 3]}],
                   {row, row1, xwing_possibility, [{1, 2, 1, [1]}, {1, 3, 1, [1]}]}), []),
    ?assertEqual(sudoku_messagismus:get_xwing_set_messages(
                   [{2, 2, 1, [1, 2]}, {2, 3, 1, [1, 3]}],
                   {row, row1, xwing_possibility, [{1, 2, 1, [1]}, {1, 3, 1, [1]}]}),
                 [{row, ignore, xwing_set,
                   [{2, 2, 1, [1]},
                    {2, 3, 1, [1]},
                    {1, 2, 1, [1]},
                    {1, 3, 1, [1]}]}]).

generate_xwing_possibility_messages_test() ->
    ?assertEqual(sudoku_messagismus:generate_xwing_possibility_messages(
                   row, row1, [{1, 2, 1, [1, 2]}, {1, 3, 1, [1, 3]}, {1, 4, 1, [4]}]),
                 [{row, row1, xwing_possibility, [{1, 2, 1, [1]}, {1, 3, 1, [1]}]}]),
    ?assertEqual(sudoku_messagismus:generate_xwing_possibility_messages(
                   col, col2, [{1, 2, 1, [1, 2]}, {2, 2, 1, [1, 3]}, {3, 2, 1, [4]}]),
                 [{col, col2, xwing_possibility, [{1, 2, 1, [1]}, {2, 2, 1, [1]}]}]),
    ?assertEqual(sudoku_messagismus:generate_xwing_possibility_messages(
                   cell, cell1, [{1, 2, 1, [1, 2]}, {2, 2, 1, [1, 3]}, {3, 2, 1, [4]}]),
                 []),
    ?assertEqual(sudoku_messagismus:generate_xwing_possibility_messages(
                   row, row1, [{1, 2, 1, [1, 2]}, {1, 3, 1, [1, 3]}, {1, 4, 1, [3]}]),
                 [{row, row1, xwing_possibility, [{1, 2, 1, [1]}, {1, 3, 1, [1]}]},
                  {row, row1, xwing_possibility, [{1, 3, 1, [3]}, {1, 4, 1, [3]}]}]).
