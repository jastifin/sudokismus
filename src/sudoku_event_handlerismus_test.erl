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
%% @doc Unit tests for sudoku_event_handlerismus.

-module(sudoku_event_handlerismus_test).

-include_lib("eunit/include/eunit.hrl").
-include("sudoku_gen_eventus.hrl").

do_event_handling_test() ->
    % test row -> row
    OldValues_R1 = [{1, C, 1, lists:seq(1, 3)} || C <- lists:seq(1, 3)] ++
                 [{1, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                 [{1, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    Expected_R1  = [{1, C, 1, [3]}            || C <- lists:seq(1, 3)] ++
                 [{1, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                 [{1, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row2, has_set, 2, [{2, 1, 1, [1, 2]}, {2, 2, 1, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R1}), Expected_R1),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row2, has_set, 2, [{2, 1, 1, [1, 2]}, {2, 4, 2, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R1}), OldValues_R1),
    % test col -> row
    Expected_R1_2  = [{1, 1, 1, [1, 2]}] ++
                 [{1, C, 1, [3]}            || C <- lists:seq(2, 3)] ++
                 [{1, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                 [{1, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 1, 1, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R1}), Expected_R1_2),
    OldValues_R2 = [{2, C, 1, lists:seq(1, 3)} || C <- lists:seq(1, 3)] ++
                   [{2, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                   [{2, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    Expected_R2  = [{2, 1, 1, [1, 2]}] ++
                   [{2, C, 1, [3]}            || C <- lists:seq(2, 3)] ++
                   [{2, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                   [{2, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 1, 1, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R2}), Expected_R2),
    OldValues_R3 = [{3, C, 1, lists:seq(1, 3)} || C <- lists:seq(1, 3)] ++
                 [{3, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                 [{3, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    Expected_R3  = [{3, C, 1, [3]}            || C <- lists:seq(1, 3)] ++
                 [{3, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                 [{3, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 1, 1, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R3}), Expected_R3),
    Expected_R1_3  = [{1, 1, 1, [1, 2]}] ++
                   [{1, C, 1, lists:seq(1, 3)} || C <- lists:seq(2, 3)] ++
                   [{1, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                   [{1, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {4, 1, 4, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R1}), Expected_R1_3),
    OldValues_R4 = [{4, C, 4, lists:seq(1, 3)} || C <- lists:seq(1, 3)] ++
                   [{4, C, 5, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                   [{4, C, 6, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    Expected_R4  = [{4, 1, 4, [1, 2]}] ++
                   [{4, C, 4, lists:seq(1, 3)} || C <- lists:seq(2, 3)] ++
                   [{4, C, 5, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                   [{4, C, 6, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {4, 1, 4, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R4}), Expected_R4),
    Expected_R3_1 = [{3, 1, 1, [3]}] ++
                 [{3, C, 1, lists:seq(1, 3)} || C <- lists:seq(2, 3)] ++
                 [{3, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                 [{3, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {4, 1, 4, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R3}), Expected_R3_1),

    % test cell -> row
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({cell, cell1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 2, 1, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R1}), Expected_R1_2),
    Expected_R2_2  = [{2, 1, 1, [3]}] ++
                     [{2, 2, 1, [1, 2]}] ++
                     [{2, 3, 1, [3]}] ++
                     [{2, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                     [{2, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({cell, cell1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 2, 1, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R2}), Expected_R2_2),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({cell, cell1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 2, 1, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R3}), Expected_R3),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({cell, cell1, has_set, 2, [{4, 1, 4, [1, 2]}, {5, 2, 4, [1, 2]}]},
                              #cell_state{type = row, values = OldValues_R3}), OldValues_R3),

    %test row -> col
    OldValues_C1 =   [{R, 1, 1, lists:seq(1, 3)} || R <- lists:seq(1, 3)] ++
                     [{R, 1, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                     [{R, 1, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    Expected_C1_1  = [{1, 1, 1, [1, 2]}] ++
                     [{R, 1, 1, [3]}            || R <- lists:seq(2, 3)] ++
                     [{R, 1, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                     [{R, 1, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row1, has_set, 2, [{1, 1, 1, [1, 2]}, {1, 2, 1, [1, 2]}]},
                              #cell_state{type = col, values = OldValues_C1}), Expected_C1_1),
    OldValues_C2 = [{R, 2, 1, lists:seq(1, 3)} || R <- lists:seq(1, 3)] ++
                   [{R, 2, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                   [{R, 2, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    Expected_C2_1  = [{1, 2, 1, [1, 2]}] ++
                     [{R, 2, 1, [3]}            || R <- lists:seq(2, 3)] ++
                     [{R, 2, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                     [{R, 2, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row1, has_set, 2, [{1, 1, 1, [1, 2]}, {1, 2, 1, [1, 2]}]},
                              #cell_state{type = col, values = OldValues_C2}), Expected_C2_1),
    OldValues_C3 = [{R, 3, 1, lists:seq(1, 3)} || R <- lists:seq(1, 3)] ++
                   [{R, 3, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                   [{R, 3, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    Expected_C3_1  = [{R, 3, 1, [3]}            || R <- lists:seq(1, 3)] ++
                     [{R, 3, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                     [{R, 3, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row1, has_set, 2, [{1, 1, 1, [1, 2]}, {1, 2, 1, [1, 2]}]},
                              #cell_state{type = col, values = OldValues_C3}), Expected_C3_1),
    Expected_C1_2  = [{1, 1, 1, [1, 2]}] ++
                 [{R, 1, 1, lists:seq(1, 3)} || R <- lists:seq(2, 3)] ++
                 [{R, 1, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                 [{R, 1, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row1, has_set, 2, [{1, 1, 1, [1, 2]}, {1, 4, 2, [1, 2]}]},
                              #cell_state{type = col, values = OldValues_C1}), Expected_C1_2),
    OldValues_C4 = [{R, 4, 2, lists:seq(1, 3)} || R <- lists:seq(1, 3)] ++
                   [{R, 4, 5, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                   [{R, 4, 8, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    Expected_C4_2  = [{1, 4, 2, [1, 2]}] ++
                 [{R, 4, 2, lists:seq(1, 3)} || R <- lists:seq(2, 3)] ++
                 [{R, 4, 5, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                 [{R, 4, 8, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row1, has_set, 2, [{1, 1, 1, [1, 2]}, {1, 4, 2, [1, 2]}]},
                              #cell_state{type = col, values = OldValues_C4}), Expected_C4_2),
    Expected_C3_2  = [{1, 3, 1, [3]}] ++
                 [{R, 3, 1, lists:seq(1, 3)} || R <- lists:seq(2, 3)] ++
                 [{R, 3, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                 [{R, 3, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row1, has_set, 2, [{1, 1, 1, [1, 2]}, {1, 4, 2, [1, 2]}]},
                              #cell_state{type = col, values = OldValues_C3}), Expected_C3_2),

    % test col -> col
    Expected_C2_2  = [{R, 2, 1, [3]}            || R <- lists:seq(1, 3)] ++
                     [{R, 2, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                     [{R, 2, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling(
                   {col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 1, 1, [1, 2]}]},
                   #cell_state{type = col, values = OldValues_C2}), Expected_C2_2),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling(
                   {col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {4, 1, 4, [1, 2]}]},
                   #cell_state{type = col, values = OldValues_C2}), OldValues_C2),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling(
                   {col, col4, has_set, 2, [{1, 4, 2, [1, 2]}, {4, 4, 5, [1, 2]}]},
                   #cell_state{type = col, values = OldValues_C2}), OldValues_C2),

    % test cell -> col
    ?assertEqual(sudoku_event_handlerismus:do_event_handling(
                   {cell, cell1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 2, 1, [1, 2]}]},
                   #cell_state{type = col, values = OldValues_C1}), Expected_C1_1),
    Expected_C2_3  = [{1, 2, 1, [3]}] ++
                     [{2, 2, 1, [1, 2]}] ++
                     [{3, 2, 1, [3]}] ++
                     [{R, 2, 4, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                     [{R, 2, 7, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling(
                   {cell, cell1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 2, 1, [1, 2]}]},
                   #cell_state{type = col, values = OldValues_C2}), Expected_C2_3),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling(
                   {cell, cell1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 2, 1, [1, 2]}]},
                   #cell_state{type = col, values = OldValues_C3}), Expected_C3_1),
    OldValues_C4 = [{R, 4, 2, lists:seq(1, 3)} || R <- lists:seq(1, 3)] ++
                   [{R, 4, 5, lists:seq(1, 3)} || R <- lists:seq(4, 6)] ++
                   [{R, 4, 8, lists:seq(4, 9)} || R <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling(
                   {cell, cell1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 2, 1, [1, 2]}]},
                   #cell_state{type = col, values = OldValues_C4}), OldValues_C4),
    OldValues_C5 = [{1, 5, 2, [4]},
                    {2, 5, 2, [2]},
                    {3, 5, 2, [6]},
                    {4, 5, 5, [1]},
                    {5, 5, 5, "\t"},
                    {6, 5, 5, [7]},
                    {7, 5, 8, "\b"},
                    {8, 5, 8, [5]},
                    {9, 5, 8, [3]}],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling(
                   {cell, cell8, has_set, 2, [{8, 5, 8, [3, 5]}, {9, 5, 8, [3, 5]}]},
                   #cell_state{type = col, values = OldValues_C5}), OldValues_C5),

    % test row -> cell
    OldValues_Z1 = lists:reverse(
                     lists:foldl(
                       fun(R, AccIn) -> lists:foldl(
                                          fun(C, AccIn2) -> [{R, C, 1, [1, 2, 3]} | AccIn2] end,
                                          AccIn, [1, 2, 3])
                       end, [], [1, 2, 3])),
    Expected_Z1  =  [{1, C, 1, [1, 2]} || C <- [1, 2]] ++
                        [{1, 3, 1, [3]}] ++
                        lists:reverse(lists:foldl(
                                        fun(R, AccIn) -> lists:foldl(
                                                           fun(C, AccIn2) -> [{R, C, 1, [3]} | AccIn2] end,
                                                           AccIn, [1, 2, 3])
                                        end, [], [2, 3])),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row1, has_set, 2, [{1, 1, 1, [1, 2]}, {1, 2, 1, [1, 2]}]},
                              #cell_state{type = cell, values = OldValues_Z1}), Expected_Z1),
    Expected_Z1_2  =  [{1, 1, 1, [1, 2]}] ++
                        [{1, C, 1, [3]} || C <- [2, 3]] ++
                        lists:reverse(lists:foldl(
                                        fun(R, AccIn) -> lists:foldl(
                                                           fun(C, AccIn2) -> [{R, C, 1, [1, 2, 3]} | AccIn2] end,
                                                           AccIn, [1, 2, 3])
                                        end, [], [2, 3])),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row1, has_set, 2, [{1, 1, 1, [1, 2]}, {1, 4, 2, [1, 2]}]},
                              #cell_state{type = cell, values = OldValues_Z1}), Expected_Z1_2),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({row, row1, has_set, 2, [{4, 1, 4, [1, 2]}, {4, 2, 4, [1, 2]}]},
                              #cell_state{type = cell, values = OldValues_Z1}), OldValues_Z1),

    % test col -> cell
    Expected_Z1_3  =
        [{1, 1, 1, [1, 2]}] ++
            [{1, C, 1, [3]} || C <- [2, 3]] ++
            [{2, 1, 1, [1, 2]}] ++
            [{2, C, 1, [3]} || C <- [2, 3]] ++
            [{3, C, 1, [3]} || C <- [1, 2, 3]],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {2, 1, 1, [1, 2]}]},
                              #cell_state{type = cell, values = OldValues_Z1}), Expected_Z1_3),
    Expected_Z1_4  =
        [{1, 1, 1, [1, 2]}] ++
            [{1, C, 1, [1, 2, 3]} || C <- [2, 3]] ++
            [{2, 1, 1, [3]}] ++
            [{2, C, 1, [1, 2, 3]} || C <- [2, 3]] ++
            [{3, 1, 1, [3]}] ++
            [{3, C, 1, [1, 2, 3]} || C <- [2, 3]],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({col, col1, has_set, 2, [{1, 1, 1, [1, 2]}, {4, 1, 4, [1, 2]}]},
                              #cell_state{type = cell, values = OldValues_Z1}), Expected_Z1_4),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({col, col1, has_set, 2, [{1, 4, 2, [1, 2]}, {2, 4, 2, [1, 2]}]},
                              #cell_state{type = cell, values = OldValues_Z1}), OldValues_Z1),

   % test cell -> cell
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({cell, cell2, has_set, 2, [{1, 4, 2, [1, 2]}, {2, 4, 2, [1, 2]}]},
                              #cell_state{type = cell, values = OldValues_Z1}), OldValues_Z1),

    % test has_in_pair: cell -> row
    Expected_R1_1 = [{1, C, 1, lists:seq(1, 2)} || C <- lists:seq(1, 3)] ++
                 [{1, C, 2, lists:seq(1, 3)} || C <- lists:seq(4, 6)] ++
                 [{1, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({cell, 1, has_in_row, 2, 3}, #cell_state{type = row, values = OldValues_R1}), Expected_R1_1),
    Expected_R2_1 = [{2, C, 1, lists:seq(1, 3)} || C <- lists:seq(1, 3)] ++
                   [{2, C, 2, lists:seq(1, 2)} || C <- lists:seq(4, 6)] ++
                   [{2, C, 3, lists:seq(4, 9)} || C <- lists:seq(7, 9)],
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({cell, 1, has_in_row, 2, 3}, #cell_state{type = row, values = OldValues_R2}), Expected_R2_1),
    ?assertEqual(sudoku_event_handlerismus:do_event_handling({cell, 1, has_in_row, 2, 3}, #cell_state{type = row, values = OldValues_R4}), OldValues_R4).
