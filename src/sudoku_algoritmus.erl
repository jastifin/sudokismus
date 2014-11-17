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
%% @doc Sudoku algorithms.

-module(sudoku_algoritmus).

-include("sudoku_gen_eventus.hrl").

-export([perform_cleanup/1,
         find_same_sets/2,
         perform_unique_cleanup/1]).

%% perform_single_cleanup(Values, OldSingle)
%% find new single numbers and remove them from other positions.

-spec perform_single_cleanup(Values, OldSingle) -> Values2 when
    Values    ::[sudoku_boardismus:element_info()],
    OldSingle ::[sudoku_boardismus:element_info()],
    Values2   ::[sudoku_boardismus:element_info()].

perform_single_cleanup(Values, OldSingle) ->
    NewSingle = lists:filter(
                  fun({_, _, _, Numbers}) ->
                          length(Numbers) == 1
                  end, Values),
    Diff      = NewSingle -- OldSingle,
    case Diff of
        [] -> Values;
        _  -> perform_single_cleanup(sudoku_boardismus:remove_and_set_values(Diff, Values), NewSingle)
    end.

%% perform_unique_cleanup(Values)
%% find unique numbers and remove other numbers from the position.

-spec perform_unique_cleanup(Values) -> Values2 when
    Values  ::[sudoku_boardismus:element_info()],
    Values2 ::[sudoku_boardismus:element_info()].

perform_unique_cleanup(Values) ->
    NewValues   = perform_single_cleanup(Values, []),
    Positions   = sudoku_boardismus:find_positions(NewValues),
    FoundUnique = lists:filtermap(
                    fun({N, List}) ->
                            case length(List) of
                                1 -> [{R, C}] = List,
                                     {R, C, Z, Numbers} = sudoku_boardismus:get_value(R, C, NewValues),
                                     case length(Numbers) of
                                         1 -> false;
                                         _ -> {true, {R, C, Z, [N]}}
                                     end;
                                _ -> false
                            end
                    end, lists:zip(lists:seq(1, 9), Positions)),

    case length(FoundUnique) of
        0 -> NewValues;
        _ -> perform_unique_cleanup(sudoku_boardismus:set_values(FoundUnique, NewValues))
    end.

perform_cleanup(Values, true) ->
    Values;
perform_cleanup(Values, false) ->
    NewValuesTmp  = perform_unique_cleanup(Values),
    NewValuesTmp2 = lists:foldl(
      fun(N, ValuesOld) ->
              SameSets = lists:foldl(
                                fun(Set, AccIn) ->
                                        AccIn ++ Set
                                end, [], find_same_sets(ValuesOld, N)),
              sudoku_boardismus:remove_and_set_values(SameSets, ValuesOld)
      end, NewValuesTmp, lists:seq(2, ?SET_MAX_SIZE)),

    perform_cleanup(NewValuesTmp2, NewValuesTmp2 =:= Values ).

%% perform_cleanup(Values)
%% top level cleanup

-spec perform_cleanup(Values) -> Values2 when
    Values  ::[sudoku_boardismus:element_info()],
    Values2 ::[sudoku_boardismus:element_info()].

perform_cleanup(Values) ->
    perform_cleanup(Values, false).

%% find_same_sets(Values, L)

-spec find_same_sets(Values, L) -> SameSets when
    Values   ::[sudoku_boardismus:element_info()],
    L        ::non_neg_integer(),
    SameSets ::[[tuple()]].

find_same_sets(Values, L) ->
    % element_infos with L numbers
    L_Sets = lists:filter(fun({_, _, _, Numbers}) -> length(Numbers) == L end, Values),

    % all element_infos of length L with same L numbers
    L_UniqueNumbers = lists:reverse(
                        lists:foldl(
                          fun({Offset, Value1}, AccIn) ->
                                  SameNumbers = sudoku_boardismus:find_same_numbers(element(4, Value1), lists:nthtail(Offset, L_Sets)),
                                  case length(SameNumbers) + 1 of
                                      L -> [[Value1 | SameNumbers] | AccIn];
                                      _ -> AccIn
                                  end
                          end, [], lists:zip(lists:seq(1, length(L_Sets)), L_Sets))),

    % {n,positions} of numbers with L positions
    L_Positions = lists:filter(
                    fun({_, List}) ->
                            length(List) == L
                    end, lists:zip(lists:seq(1, 9), sudoku_boardismus:find_positions(Values))),

    SameSets = case length(L_Positions) > 0 of
                   true ->
                       lists:filtermap(
                         fun({Offset, {N, Positions}}) ->
                                 List  = sudoku_boardismus:find_same_positions(Positions, lists:nthtail(Offset, L_Positions)),
                                 case length(List) + 1 of
                                     L -> Numbers = [N | lists:map(fun(Tuple) -> element(1, Tuple) end, List)],
                                          Set = lists:map(
                                                  fun({R, C}) ->
                                                          setelement(4, sudoku_boardismus:get_value(R, C, Values), Numbers)
                                                  end, Positions),
                                          case lists:member(Set, L_UniqueNumbers) of
                                              false -> {true, Set};
                                              true  -> false
                                          end;
                                     _               -> false
                                 end
                         end, lists:zip(lists:seq(1, length(L_Positions)-1), lists:sublist(L_Positions, length(L_Positions)-1)));
                   false -> []
               end,
    L_UniqueNumbers ++ SameSets.
