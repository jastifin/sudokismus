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
%% @doc Sudoku application supervisor

-module(sudoku_supitus).

-behaviour(supervisor).

-export([start_link/1, init/1]).

%% start_link(Layout)

-spec start_link(Layout) -> {ok, pid()} | ignore | {error, term()} when
    Layout ::[sudoku_boardismus:element_info()].

start_link(Layout) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Layout).

%% init(Args)

-spec init(Args) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
                          [supervisor:child_spec()]}} | ignore when
    Args ::[sudoku_boardismus:element_info()].

init(_Args) ->
    RowChildren = lists:map(
                    fun(RowId) ->
                            {RowId, {sudoku_gen_eventus, start, [{{row, RowId}}]},
                             permanent, 5000, worker, dynamic}
                    end, sudoku_boardismus:get_row_ids()),
    ColChildren = lists:map(
                    fun(ColId) ->
                            {ColId, {sudoku_gen_eventus, start, [{{col, ColId}}]},
                             permanent, 5000, worker, dynamic}
                    end, sudoku_boardismus:get_col_ids()),
    CellChildren = lists:map(
                     fun(CellId) ->
                             {CellId, {sudoku_gen_eventus, start, [{{cell, CellId}}]},
                              permanent, 5000, worker, dynamic}
                     end, sudoku_boardismus:get_cell_ids()),
    {ok, {{one_for_all, 2, 60}, RowChildren ++ ColChildren ++ CellChildren}}.
