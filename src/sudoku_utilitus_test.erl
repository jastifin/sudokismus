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
%% @doc Unit tests for sudoku_utilitus.


-module(sudoku_utilitus_test).

-include_lib("eunit/include/eunit.hrl").

remove_duplicates_test() ->
    ?assertEqual(sudoku_utilitus:remove_duplicates([1, 2, 3]), [1, 2, 3]),
    ?assertEqual(sudoku_utilitus:remove_duplicates([]), []),
    ?assertEqual(sudoku_utilitus:remove_duplicates([1, 2, 2, 3]), [1, 2, 3]),
    ?assertEqual(sudoku_utilitus:remove_duplicates([1, 2, 2, 1, 1]), [1, 2]),
    ?assertEqual(sudoku_utilitus:remove_duplicates([{cell1, has_in_row, 2, 1},
                                                {cell1, has_in_row, 2, 2},
                                                {cell1, has_in_row, 2, 1}]),
                 [{cell1, has_in_row, 2, 1},{cell1, has_in_row, 2, 2}]).

increment_counts_test() ->
    ?assertEqual(sudoku_utilitus:increment_counts(lists:duplicate(9, 0),[]), lists:duplicate(9, 0)),
    ?assertEqual(sudoku_utilitus:increment_counts(lists:duplicate(9, 0),[1, 2, 3, 4, 5, 6, 7, 8, 9]), lists:duplicate(9, 1)),
    ?assertEqual(sudoku_utilitus:increment_counts(lists:duplicate(9, 1),[1, 2, 3, 4, 5, 6, 7, 8, 9]), lists:duplicate(9, 2)),
    ?assertEqual(sudoku_utilitus:increment_counts(lists:duplicate(9, 0),[1]), [1 | lists:duplicate(8, 0)]),
    ?assertEqual(sudoku_utilitus:increment_counts(lists:duplicate(9, 2),[1, 2]), [3 | [3 | lists:duplicate(7, 2)]]).
