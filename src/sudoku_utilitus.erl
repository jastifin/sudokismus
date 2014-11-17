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
%% @doc sudoku utilities

-module(sudoku_utilitus).

-export([remove_duplicates/1,
         increment_counts/2]).

%% remove_duplicates(List1)

-spec remove_duplicates(List1) -> List2 when
    List1 ::[any()],
    List2 ::[any()].

remove_duplicates(List) ->
    lists:reverse(
      lists:foldl(
        fun(E, AccIn) ->
                case lists:member(E, AccIn) of true -> AccIn; false -> [E | AccIn] end
        end, [], List)).

%% increment_counts(OldCounts, Numbers)
%%

-spec increment_counts(OldCounts, Numbers) -> Counts when
    OldCounts ::[integer()],
    Numbers   ::[integer()],
    Counts    ::[integer()].

increment_counts(OldCounts, Numbers) ->
    lists:map(
      fun(N) ->
              case lists:member(N, Numbers) of
                  true  -> lists:nth(N, OldCounts) + 1;
                  false -> lists:nth(N, OldCounts)
              end
      end, lists:seq(1, 9)).

