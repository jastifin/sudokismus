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
%% @doc unit tests for sudoku_algoritmus.

-module(sudoku_algoritmus_test).

-include_lib("eunit/include/eunit.hrl").

perform_cleanup_test() ->
    ?assertEqual(sudoku_algoritmus:perform_cleanup([]), []),

    Values3 = [{1, 1, 0, [3, 6]},
                        {1, 2, 0, [1, 3, 4, 7, 9]},
                        {1, 3, 0, [2, 3, 4, 7, 8]},
                        {1, 4, 0, [1, 2, 3, 4, 7, 8, 9]},
                        {1, 5, 0, [2, 3, 4, 6, 7, 8, 9]},
                        {1, 6, 0, [1, 2, 3, 4, 6, 7, 8]},
                        {1, 7, 0, [1, 2, 3, 6, 8, 9]},
                        {2, 1, 0, [3, 5, 6]},
                        {2, 2, 0, [3, 4, 5, 7, 9]}],
    Expected3 = [{1, 1, 0, [3]},
                        {1, 2, 0, [1, 4, 7, 9]},
                        {1, 3, 0, [2, 4, 7, 8]},
                        {1, 4, 0, [1, 2, 4, 7, 8, 9]},
                        {1, 5, 0, [2, 4, 6, 7, 8, 9]},
                        {1, 6, 0, [1, 2, 4, 6, 7, 8]},
                        {1, 7, 0, [1, 2, 6, 8, 9]},
                        {2, 1, 0, [5]},
                        {2, 2, 0, [4, 7, 9]}],
    ?assertEqual(sudoku_algoritmus:perform_cleanup(sudoku_boardismus:remove_from_col(1, [6], Values3)), Expected3),

    Values4 = [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 2]}] ++ [ {R, 1, 2, lists:seq(2, 9)} || R <- lists:seq(3, 9)],
    Expected4 = [{1, 1, 2, [1, 2]}, {2, 1, 2, [1, 2]}] ++ [ {R, 1, 2, lists:seq(3, 9)} || R <- lists:seq(3, 9)],
    ?assertEqual(sudoku_algoritmus:perform_cleanup(Values4), Expected4),
    Values5 = [{1, 1, 1, [4, 6]}, {1, 2, 1, [7]}, {1, 3, 1, [3]},
               {1, 4, 2, [2]}, {1, 5, 2, [8]}, {1, 6, 2, [1, 6]},
               {1, 7, 3, [1, 9]}, {1, 8, 3, [5]}, {1, 9, 3, [1, 4, 6, 9]}],
    Expected5 = [{1, 1, 1, [4, 6]}, {1, 2, 1, [7]}, {1, 3, 1, [3]},
                              {1, 4, 2, [2]}, {1, 5, 2, [8]}, {1, 6, 2, [1, 6]},
                              {1, 7, 3, [1, 9]}, {1, 8, 3, [5]}, {1, 9, 3, [1, 4, 6, 9]}],
    ?assertEqual(sudoku_algoritmus:perform_cleanup(Values5), Expected5).

perform_unique_cleanup_test() ->
    Values1 = [{1, 1, 2, [1]}, {1, 2, 2, [1, 3]}, {1, 3, 2, [1, 3, 4, 5]}, {1, 4, 2, [1, 4, 5]}],
    Expected1 = [{1, 1, 2, [1]}, {1, 2, 2, [3]}, {1, 3, 2, [4, 5]}, {1, 4, 2, [4, 5]}],
    ?assertEqual(sudoku_algoritmus:perform_unique_cleanup(Values1), Expected1),

    Values2   = [{1, 1, 0, [1, 2]}, {1, 2, 0, [1, 3]}],
    Expected2 = [{1, 1, 0, [2]}, {1, 2, 0, [3]}],
    ?assertEqual(sudoku_algoritmus:perform_unique_cleanup(Values2), Expected2).

find_same_sets_test() ->
    ?assertEqual(sudoku_algoritmus:find_same_sets([], 3), []),
    ?assertEqual(sudoku_algoritmus:find_same_sets([{1, 2, 3, [1, 2, 3]}], 3), []),
    ?assertEqual(sudoku_algoritmus:find_same_sets([{1, 2, 3, [1, 2, 3]}, {2, 3, 3, [1, 2, 3]}], 3), []),
    Res1 = sudoku_algoritmus:find_same_sets([{1, 2, 3, [1, 2, 3]}, {2, 3, 3, [1, 2, 3]}, {3, 4, 3, [1, 2, 3]}], 3),
    ?assertEqual(Res1, [[{1, 2, 3, [1, 2, 3]}, {2, 3, 3, [1, 2, 3]}, {3, 4, 3, [1, 2, 3]}]]),
    Res2 = sudoku_algoritmus:find_same_sets([{1, 2, 3, [1, 2, 3]}, {2, 3, 3, [1, 2, 3]}, {3, 4, 3, [1, 2, 3, 4]}], 3),
    ?assertEqual(Res2, [[{1, 2, 3, [1, 2, 3]}, {2, 3, 3, [1, 2, 3]}, {3, 4, 3, [1, 2, 3]}]]),

    ?assertEqual(sudoku_algoritmus:find_same_sets([], 5), []),
    ?assertEqual(sudoku_algoritmus:find_same_sets([{1, 2, 3, [1, 2, 3]}], 5), []),
    ?assertEqual(sudoku_algoritmus:find_same_sets([{1, 2, 3, [1, 2, 3]}, {2, 3, 3, [1, 2, 3]}], 5), []),
    Res3 = sudoku_algoritmus:find_same_sets(
             [{1, 2, 3, [1, 2, 3, 4, 5]}, {2, 3, 3, [1, 2, 3, 5, 4]}, {3, 4, 3, [1, 2, 3, 4, 5, 6]},
              {4, 5, 6, [1, 2, 3, 4, 5]}, {5, 6, 5, [1, 2, 3, 4, 5]}],
             5),
    ?assertEqual(Res3, [[{1, 2, 3, [1, 2, 3, 4, 5]}, {2, 3, 3, [1, 2, 3, 4, 5]}, {3, 4, 3, [1, 2, 3, 4, 5]},
                         {4, 5, 6, [1, 2, 3, 4, 5]}, {5, 6, 5, [1, 2, 3, 4, 5]}]]).
