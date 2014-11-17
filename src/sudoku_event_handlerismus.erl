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
%% @doc sudoku event handler.

-module(sudoku_event_handlerismus).

-include_lib("eunit/include/eunit.hrl").
-include("sudoku_gen_eventus.hrl").

-export([do_event_handling/2]).

%% do_event_handling
%% process event
-spec do_event_handling(Event, State) -> Values when
    Event  ::tuple(),
    State  ::#cell_state{},
    Values ::[sudoku_boardismus:element_info()].

do_event_handling({row, _Id, has, {_Row, Col, Zone, Numbers}}, #cell_state{type = row, values = Values})->
    sudoku_boardismus:remove_from_zone(Zone, Numbers, sudoku_boardismus:remove_from_col(Col, Numbers, Values));

do_event_handling({row, _Id, has, {Row, Col, Zone, Numbers}}, #cell_state{type = col, values = Values}) ->
    case sudoku_boardismus:get_col(Values) of
        Col -> sudoku_boardismus:remove_and_set_values([{Row, Col, Zone, Numbers}], Values);
        _   -> sudoku_boardismus:remove_from_zone(Zone, Numbers, sudoku_boardismus:remove_from_row(Row, Numbers, Values))
    end;

do_event_handling({row, _Id, has, {Row, Col, Zone, Numbers}}, #cell_state{type = cell, values = Values}) ->
    case sudoku_boardismus:get_cell(Values) of
        Zone -> sudoku_boardismus:remove_and_set_values([{Row, Col, Zone, Numbers}], Values);
        _    -> sudoku_boardismus:remove_from_col(Col, Numbers, sudoku_boardismus:remove_from_row(Row, Numbers, Values))
    end;

do_event_handling({col, _Id, has, {Row, Col, Zone, Numbers}}, #cell_state{type = row, values = Values}) ->
    case sudoku_boardismus:get_row(Values) of
        Row -> sudoku_boardismus:remove_and_set_values([{Row, Col, Zone, Numbers}], Values);
        _   -> sudoku_boardismus:remove_from_zone(Zone, Numbers, sudoku_boardismus:remove_from_col(Col, Numbers, Values))
    end;

do_event_handling({col, _Id, has, {Row, _Col, Zone, Numbers}}, #cell_state{type = col, values = Values}) ->
    sudoku_boardismus:remove_from_zone(Zone, Numbers, sudoku_boardismus:remove_from_row(Row, Numbers, Values));

do_event_handling({col, _Id, has, {Row, Col, Zone, Numbers}}, #cell_state{type = cell, values = Values}) ->
    case sudoku_boardismus:get_cell(Values) of
        Zone -> sudoku_boardismus:remove_and_set_values([{Row, Col, Zone, Numbers}], Values);
        _    -> sudoku_boardismus:remove_from_col(Col, Numbers, sudoku_boardismus:remove_from_row(Row, Numbers, Values))
    end;

do_event_handling({cell, _Id, has, {Row, Col, Zone, Numbers}}, #cell_state{type = row, values = Values}) ->
    case sudoku_boardismus:get_row(Values) of
        Row -> sudoku_boardismus:remove_and_set_values([{Row, Col, Zone, Numbers}], Values);
        _   -> sudoku_boardismus:remove_from_zone(Zone, Numbers, sudoku_boardismus:remove_from_col(Col, Numbers, Values))
    end;

do_event_handling({cell, _Id, has, {Row, Col, Zone, Numbers}}, #cell_state{type = col, values = Values}) ->
    case sudoku_boardismus:get_col(Values) of
        Col -> sudoku_boardismus:remove_and_set_values([{Row, Col, Zone, Numbers}], Values);
        _   -> sudoku_boardismus:remove_from_zone(Zone, Numbers, sudoku_boardismus:remove_from_row(Row, Numbers, Values))
    end;

do_event_handling({cell, _Id, has, {Row, Col, Zone, Numbers}}, #cell_state{type = cell, values = Values}) ->
    ?assertNotEqual(sudoku_boardismus:get_cell(Values), Zone),
    sudoku_boardismus:remove_from_col(Col, Numbers, sudoku_boardismus:remove_from_row(Row, Numbers, Values));

do_event_handling({row, _Id, has_set, _N, Positions}, #cell_state{type = row, values = Values}) ->
    {_, _, Zone, Numbers} = lists:nth(1, Positions),

    case lists:all(fun({_, _, Z, _}) -> Z == Zone end, Positions) of
        true  -> sudoku_boardismus:remove_from_zone(Zone, Numbers, Values);
        false -> Values
    end;

do_event_handling({row, _Id, has_set, _N, Positions}, #cell_state{type = col, values = Values}) ->
    {Row, _, Zone, Numbers} = lists:nth(1, Positions),
    Removed                 = case lists:all(fun({_, _, Z, _}) -> Z == Zone end, Positions) of
                                  true  -> sudoku_boardismus:remove_from_zone(Zone, Numbers, Values);
                                  false -> Values
                              end,

    case lists:keyfind(sudoku_boardismus:get_col(Values), 2, Positions) of
        false        -> sudoku_boardismus:remove_from_row(Row, Numbers, Removed);
        {_, C, Z, _} -> sudoku_boardismus:set_lesser_values([{Row, C, Z, Numbers}], [lists:keyfind(Row, 1, Values)], Removed)
    end;

do_event_handling({row, _Id, has_set, _N, Positions}, #cell_state{type = cell, values = Values}) ->
    {Row, _, Zone, Numbers} = lists:nth(1, Positions),
    Removed                 = case lists:all(fun({_, _, Z, _}) -> Z == Zone end, Positions) of
                                  true  -> sudoku_boardismus:remove_from_zone(Zone, Numbers, Values);
                                  false -> sudoku_boardismus:remove_from_row(Row, Numbers, Values)
                              end,
    sudoku_boardismus:set_lesser_values(Positions, lists:map(
                        fun({R, C, _, _}) ->
                                sudoku_boardismus:get_value(R, C, Values)
                        end, Positions), Removed);

do_event_handling({col, _Id, has_set, _N, Positions}, #cell_state{type = row, values = Values}) ->
    {_, Col, Zone, Numbers} = lists:nth(1, Positions),

    Removed = case lists:all(fun({_, _, Z, _}) -> Z == Zone end, Positions) of
                  true  -> sudoku_boardismus:remove_from_zone(Zone, Numbers, Values);
                  false -> Values
              end,

    case lists:keyfind(sudoku_boardismus:get_row(Values), 1, Positions) of
        false        -> sudoku_boardismus:remove_from_col(Col, Numbers, Removed);
        {R, _, Z, _} -> sudoku_boardismus:set_lesser_values([{R, Col, Z, Numbers}], [lists:keyfind(Col, 2, Values)], Removed)
    end;

do_event_handling({col, _Id, has_set, _N, Positions}, #cell_state{type = col, values = Values}) ->
    {_, _, Zone, Numbers} = lists:nth(1, Positions),
    case lists:all(fun({_, _, Z, _}) -> Z == Zone end, Positions) of
        true  -> sudoku_boardismus:remove_from_zone(Zone, Numbers, Values);
        false -> Values
    end;

do_event_handling({col, _Id, has_set, _N, Positions}, #cell_state{type = cell, values = Values}) ->
    {_, Col, Zone, Numbers} = lists:nth(1, Positions),
    Removed                 = case lists:all(fun({_, _, Z, _}) -> Z == Zone end, Positions) of
                                  true  -> sudoku_boardismus:remove_from_zone(Zone, Numbers, Values);
                                  false -> sudoku_boardismus:remove_from_col(Col, Numbers, Values)
                              end,
    sudoku_boardismus:set_lesser_values(Positions, lists:map(
                        fun({R, C, _, _}) ->
                                sudoku_boardismus:get_value(R, C, Values)
                        end, Positions), Removed);

do_event_handling({cell, _Id, has_set, N, Positions}, #cell_state{type = row, values = Values}) ->
    {_, _, Zone, Numbers} = lists:nth(1, Positions),
    MyRow                 = sudoku_boardismus:get_row(Values),
    SameRow               = lists:filter(fun({R, _, _, _}) -> MyRow == R end, Positions),

    case length(SameRow) of
        N -> sudoku_boardismus:set_lesser_values(SameRow,
                               lists:map(fun({_, C, _, _}) -> lists:keyfind(C, 2, Values) end, SameRow),
                               sudoku_boardismus:remove_from_row(MyRow, Numbers, Values));
        _ -> sudoku_boardismus:set_lesser_values(SameRow,
                               lists:map(fun({_, C, _, _}) -> lists:keyfind(C, 2, Values) end, SameRow),
                               sudoku_boardismus:remove_from_zone(Zone, Numbers, Values))
    end;

do_event_handling({cell, _Id, has_set, N, Positions}, #cell_state{type = col, values = Values}) ->
    {_, _, Zone, Numbers} = lists:nth(1, Positions),
    MyCol                 = sudoku_boardismus:get_col(Values),
    SameCol               = lists:filter(fun({_, C, _, _}) -> MyCol == C end, Positions),

    case length(SameCol) of
        N -> sudoku_boardismus:set_lesser_values(SameCol,
                               lists:map(fun({R, _, _, _}) -> lists:keyfind(R, 1, Values) end, SameCol),
                               sudoku_boardismus:remove_from_col(MyCol, Numbers, Values));
        _ -> sudoku_boardismus:set_lesser_values(SameCol,
                               lists:map(fun({R, _, _, _}) -> lists:keyfind(R, 1, Values) end, SameCol),
                               sudoku_boardismus:remove_from_zone(Zone, Numbers, Values))
    end;

do_event_handling({cell, _Id, has_set, _N, Positions}, #cell_state{type = cell, values = Values}) ->
    {Row, Col, Zone, Numbers} = lists:nth(1, Positions),
    ?assertNotEqual(sudoku_boardismus:get_cell(Values), Zone),
    case lists:all(fun({_, C, _, _}) -> C == Col end, Positions) of
        true  -> sudoku_boardismus:remove_from_col(Col, Numbers, Values);
        false -> case lists:all(fun({R, _, _, _}) -> R == Row end, Positions) of
                    true  -> sudoku_boardismus:remove_from_row(Row, Numbers, Values);
                    false -> Values
                end
    end;

do_event_handling({cell, Zone, has_in_row, Row, N}, #cell_state{type = row, values = Values}) ->
    case sudoku_boardismus:get_row(Values) of
        Row -> Zones = sudoku_boardismus:extract_zones(Values),
               lists:foldl(
                 fun(Z, AccIn) ->
                         sudoku_boardismus:remove_from_zone(Z, [N], AccIn)
                 end, Values, lists:delete(Zone, Zones));
        _   -> sudoku_boardismus:remove_from_zone(Zone, [N], Values)
    end;

do_event_handling({cell, Zone, has_in_row, Row, N}, #cell_state{type = col, values = Values}) ->
    case element(3, lists:keyfind(Row, 1, Values)) of
        Zone -> Values;
        _    -> sudoku_boardismus:remove_from_row(Row, [N], Values)
    end;

do_event_handling({cell, Zone, has_in_row, Row, N}, #cell_state{type = cell, values = Values}) ->
    ?assertNotEqual(Zone, sudoku_boardismus:get_cell(Values)),
    sudoku_boardismus:remove_from_row(Row, [N], Values);

do_event_handling({cell, Zone, has_in_col, Col, N}, #cell_state{type = row, values = Values}) ->
    case element(3, lists:keyfind(Col, 2, Values)) of
        Zone -> Values;
        _    -> sudoku_boardismus:remove_from_col(Col, [N], Values)
    end;

do_event_handling({cell, Zone, has_in_col, Col, N}, #cell_state{type = col, values = Values}) ->
    case sudoku_boardismus:get_col(Values) of
        Col -> Zones = sudoku_boardismus:extract_zones(Values),
               lists:foldl(fun(Z, AccIn) -> sudoku_boardismus:remove_from_zone(Z, [N], AccIn) end, Values, lists:delete(Zone, Zones));
        _   -> sudoku_boardismus:remove_from_zone(Zone, [N], Values)
    end;

do_event_handling({cell, Zone, has_in_col, Col, N}, #cell_state{type = cell, values = Values}) ->
    ?assertNotEqual(Zone, sudoku_boardismus:get_cell(Values)),
    sudoku_boardismus:remove_from_col(Col, [N], Values);

do_event_handling({start}, State) ->
    State#cell_state.values;

do_event_handling({set, NewValues}, #cell_state{values = Values}) ->
    sudoku_boardismus:set_values(NewValues, Values);

do_event_handling(Event, State) ->
    io:format("~p, event: ~p~n",[State, Event]),
    error(why_here).
