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
%% @doc sudoku messaging.

-module(sudoku_messagismus).
-include("sudoku_gen_eventus.hrl").

-export([generate_msgs/3,
         send_others/2,
         generate_cell_msgs/1]).

%% send_others(ThisCell, Msgs)

-spec send_others(ThisCell, Msgs) -> ok when
    ThisCell ::atom(),
    Msgs     ::[term()].

send_others(ThisCell, Msgs) ->
    OtherCells = sudoku_boardismus:get_other_rows(ThisCell) ++ sudoku_boardismus:get_other_cols(ThisCell) ++ sudoku_boardismus:get_other_cells(ThisCell),
    lists:foreach(
      fun(Msg) ->
%%               io:format("send msg: ~p~n", [Msg]),
%%               case element(2, Msg) of
%%                   has_in_col -> io:format("~p~n", [Msg]);
%%                   has_in_row -> io:format("~p~n", [Msg]);
%%                   _ -> ignored
%%               end,
              lists:foreach(
                fun(OtherCell) ->
                        gen_event:notify(OtherCell, Msg)
                end, OtherCells)
      end, Msgs ).

-spec generate_msgs(Values, CellType, CellId) -> Msgs when
    Values   ::[sudoku_boardismus:element_info()],
    CellType ::row | col | zone,
    CellId   ::atom(),
    Msgs     ::[tuple()].

generate_msgs(Values, CellType, CellId) ->
    HasMsgs = lists:foldl(
                fun(N, AccIn) ->
                        Nlist = lists:filter(
                                  fun({_, _, _, Numbers}) ->
                                          Numbers =:= [N]
                                  end, Values),
                        case length(Nlist) of
                            1 -> [{CellType, CellId, has, lists:nth(1, Nlist)} | AccIn];
                            _ -> AccIn
                        end
                end, [], lists:seq(1, 9)),
    CellMsgs = case CellType of
                   cell -> generate_cell_msgs(Values);
                   _    -> []
               end,
    lists:foldl(
      fun(N, Msgs) ->
              lists:foldl(
                fun(Set, AccIn) ->
                        [{CellType, CellId, has_set, N, Set} | AccIn]
                end, Msgs, sudoku_algoritmus:find_same_sets(Values, N))
      end, HasMsgs ++ CellMsgs, lists:seq(2, ?SET_MAX_SIZE)).

%% generate_cell_msgs(Values)

-spec generate_cell_msgs(Values) -> Msgs when
    Values ::[sudoku_boardismus:element_info()],
    Msgs   ::[tuple()].

generate_cell_msgs(Values) ->
    Rows = sudoku_boardismus:extract_rows(Values),
    Cols = sudoku_boardismus:extract_cols(Values),
    AllRowCounts = lists:map(fun(Row) -> sudoku_boardismus:count_numbers_in_row(Values, Row) end, Rows),
    AllColCounts = lists:map(fun(Col) -> sudoku_boardismus:count_numbers_in_col(Values, Col) end, Cols),
    HasRows = lists:map(
                   fun(N) ->
                           lists:filter(fun({_, Counts}) ->lists:nth(N, Counts) /= 0 end, lists:zip(Rows, AllRowCounts))
                   end, lists:seq(1, 9)),
    HasCols = lists:map(
                   fun(N) ->
                           lists:filter(fun({_, Counts}) ->lists:nth(N, Counts) /= 0 end, lists:zip(Cols, AllColCounts))
                   end, lists:seq(1, 9)),
    MsgsRow = lists:foldl(
                fun({N, HasInRows}, AccIn) ->
                        case length(HasInRows) == 1 andalso lists:nth(N, element(2, lists:nth(1, HasInRows))) > 1 of
                            true  -> [{cell, sudoku_boardismus:get_cell(Values), has_in_row, element(1, lists:nth(1, HasInRows)), N} | AccIn];
                            false -> AccIn
                        end
                end, [], lists:zip(lists:seq(1, 9), HasRows)),
    MsgsCol = lists:foldl(
                fun({N, HasInCols}, AccIn) ->
                        case length(HasInCols) == 1 andalso lists:nth(N, element(2, lists:nth(1, HasInCols))) > 1 of
                            true  -> [{cell, sudoku_boardismus:get_cell(Values), has_in_col, element(1, lists:nth(1, HasInCols)), N} | AccIn];
                            false -> AccIn
                        end
                end, [], lists:zip(lists:seq(1, 9), HasCols)),
    MsgsRow ++ MsgsCol.
