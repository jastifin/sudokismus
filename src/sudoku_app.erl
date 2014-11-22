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
%% @doc Sudoku application

-module(sudoku_app).

-behaviour(application).

-export([start/2,
         stop/1,
         set_values/1,
         dump/0,
         dump_state/0]).

-spec start(normal, term()) -> {ok, pid()} | {error, term()}.
start(normal, _StartArgs) ->
    sudoku_supitus:start_link([]).

%% set_values(Values)

-spec set_values(Values) -> ok | failed when
    Values   ::{file, string()} |
               {values, [sudoku_boardismus:element_info()]} |
               {partial, [sudoku_boardismus:element_info()] }.

set_values({file, Filename})->
    Layout = sudoku_boardismus:read_file(Filename),
    case Layout of
        {error, Reason} -> io:format("error: ~p~n", [Reason]);
        _               -> set_values({values, Layout})
    end;
set_values({values, Layout})->
    RowIds  = sudoku_boardismus:get_row_ids(),
    ColIds  = sudoku_boardismus:get_col_ids(),
    CellIds = sudoku_boardismus:get_cell_ids(),

    lists:foreach(
      fun({Idx, RowId}) ->
              L = lists:filter(
                    fun({R, _C, _Id, _Values}) ->
                            Idx == R
                    end, Layout),
              gen_event:call(RowId, sudoku_gen_eventus, {init, lists:reverse(L)})
      end, lists:zip(lists:seq(1, 9), RowIds)),

    lists:foreach(
      fun({Idx, ColId}) ->
              L = lists:filter(
                    fun({_R, C, _Id, _Values}) ->
                            Idx == C
                    end, Layout),
              gen_event:call(ColId, sudoku_gen_eventus, {init, lists:reverse(L)})
      end, lists:zip(lists:seq(1, 9), ColIds)),

    lists:foreach(
      fun({Idx, CellId}) ->
              L = lists:filter(
                    fun({_R, _C, Id, _Values}) ->
                            Idx == Id
                    end, Layout),
              gen_event:call(CellId, sudoku_gen_eventus, {init, lists:reverse(L)})
      end, lists:zip(lists:seq(1, 9), CellIds)),

    lists:foreach(
      fun(CellId) ->
              gen_event:notify(CellId, {start})
      end, RowIds ++ ColIds ++ CellIds),

    ok;

set_values({partial, Values}) ->
    RowIds  = sudoku_boardismus:get_row_ids(),
    ColIds  = sudoku_boardismus:get_col_ids(),
    CellIds = sudoku_boardismus:get_cell_ids(),
    lists:foreach(
      fun(CellId) ->
              gen_event:notify(CellId, {set, Values})
      end, RowIds ++ ColIds ++ CellIds),
    ok.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

dump() ->
    Data = lists:map(
          fun(CellId) ->
                  RowValues = gen_event:call(CellId, sudoku_gen_eventus, {dump}),
                  {sudoku_boardismus:get_row(RowValues), RowValues}
          end, sudoku_boardismus:get_row_ids()),
    B = lists:map(
          fun(Row) ->
                  {Row, Values} = lists:keyfind(Row, 1, Data),
                  lists:map(
                    fun(Col) ->
                            {Row, Col, Zone, Numbers} = sudoku_boardismus:get_value(Row, Col, Values),
                            lists:flatten(
                              io_lib:format("~w:~s", [Zone, io_lib:write(Numbers)])) -- "[]"
                    end, lists:seq(1, 9))
          end, lists:seq(1, 9)),

    lists:foreach(
      fun(Line) ->
              io:format("~s~n", [string:join(Line, ";")])
      end, B).

dump_state() ->
    lists:map(
      fun(CellId) ->
              {CellId, gen_event:call(CellId, sudoku_gen_eventus, {dump_state})}
      end, sudoku_boardismus:get_row_ids() ++ sudoku_boardismus:get_col_ids() ++ sudoku_boardismus:get_cell_ids()).
