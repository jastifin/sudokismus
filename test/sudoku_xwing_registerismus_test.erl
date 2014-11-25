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
%% @doc Unit tests for sudoku_xwing_registerismus


-module(sudoku_xwing_registerismus_test).

-include_lib("eunit/include/eunit.hrl").

reset_test() ->
    Reg = sudoku_xwing_registerismus:reset(),
    ?assertEqual(Reg, {xwing_register, []}),
    ?assertEqual(sudoku_xwing_registerismus:reset(Reg), {xwing_register, []}).

update_register_test() ->
    Msgs1 = [{col, col1, xwing_possibility, [{1, 2, 1, [4]}, {2, 2, 1, [4]}]}],
    Msgs2 = [{col, col1, xwing_possibility, [{1, 3, 1, [5]}, {2, 3, 1, [5]}]}],
    R = sudoku_xwing_registerismus:update_register([], sudoku_xwing_registerismus:reset()),
    ?assertEqual(sudoku_xwing_registerismus:get_xwing_possibility_messages(R), []),

    R2 = sudoku_xwing_registerismus:update_register(Msgs2, R),
    ?assertEqual(sudoku_xwing_registerismus:get_xwing_possibility_messages(R2), Msgs2),

    ROut = lists:foldl(
             fun(_, RegIn) ->
                     RegOut = sudoku_xwing_registerismus:update_register(Msgs1, RegIn),
                     ?assertEqual(sudoku_xwing_registerismus:get_xwing_possibility_messages(RegOut), Msgs1),
                     RegOut
             end, R2, lists:seq(1, 100)),

    R3 = sudoku_xwing_registerismus:update_register(Msgs1, ROut),
    ?assertEqual(sudoku_xwing_registerismus:get_xwing_possibility_messages(R3), []),
    ?assertEqual(element(2, R3), [{{col, col1, xwing_possibility, [{1, 2, 1, [4]}, {2, 2, 1, [4]}]}, 101}]),

    R4 = sudoku_xwing_registerismus:update_register(Msgs1 ++ Msgs2, R3),
    ?assertEqual(sudoku_xwing_registerismus:get_xwing_possibility_messages(R4), Msgs2).
