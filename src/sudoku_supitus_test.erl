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
%% @doc Unit tests for sudoku_supitus.

-module(sudoku_supitus_test).
-include_lib("eunit/include/eunit.hrl").

-export([]).

init_test() ->
    Ret = sudoku_supitus:init([{1, 2, 3, lists:seq(1, 9)},
                               {1, 3, 5, lists:seq(1, 9)},
                               {2, 3, 4, lists:seq(1, 9)}]),
    ?assert(length(element(2, element(2, Ret))) == 3 * 9),
    ?assert(lists:keyfind(row1, 1, element(2, element(2, Ret))) =:=
                {row1,
                 {sudoku_gen_eventus, start, [{{row, row1}}]},
                 permanent, 5000, worker, dynamic}),
    ?assert(lists:keyfind(col2, 1, element(2, element(2, Ret))) =:=
                {col2,
                 {sudoku_gen_eventus, start, [{{col, col2}}]},
                 permanent, 5000, worker, dynamic}),
    ?assert(lists:keyfind(cell5, 1, element(2, element(2, Ret))) =:=
                {cell5,
                 {sudoku_gen_eventus, start, [{{cell, cell5}}]},
                 permanent, 5000, worker, dynamic}).
