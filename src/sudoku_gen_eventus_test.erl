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
%% @doc Unit tests for sudoku_gen_eventus.

-module(sudoku_gen_eventus_test).

-include_lib("eunit/include/eunit.hrl").
-include("sudoku_gen_eventus.hrl").

init_test() ->
    ?assertEqual(sudoku_gen_eventus:init({{row, row2}}),
                {ok, #cell_state{type = row, id = row2, values = []}}),
    ?assertEqual(sudoku_gen_eventus:init({{col, col2}}),
                {ok, #cell_state{type = col, id = col2, values = []}}),
    ?assertEqual(sudoku_gen_eventus:init({{cell, cell2}}),
                {ok, #cell_state{type = cell, id = cell2, values = []}}).
