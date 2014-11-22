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
%% @doc Sudoku xwing register

-module(sudoku_xwing_registerismus).

-include("sudoku_gen_eventus.hrl").

-record(xwing_register, {
    xwing_messages_info = [] ::list({Msg::tuple(), Count::non_neg_integer()})
    }).

-define(MAX_MSG_COUNT, 100).

-export([reset/0,
         reset/1,
         update_register/2,
         get_xwing_possibility_messages/1]).

%% reset()
%% Creates a new xwing message register.

-spec reset() -> #xwing_register{}.

reset() ->
    #xwing_register{xwing_messages_info=[]}.

%% reset(XwingRegister)
%% resets the xwing message register.
%% Currently same as reset/0.

-spec reset(XwingRegister) -> #xwing_register{} when
    XwingRegister ::#xwing_register{}.

reset(_XwingRegister) ->
    reset().

%% update_register(ProposedXWingMsgs, XwingRegister)
%% Updates the xwing_possibility -messages.

-spec update_register(ProposedXWingMsgs, XwingRegister) -> #xwing_register{} when
    ProposedXWingMsgs ::[tuple()],
    XwingRegister     ::#xwing_register{}.

update_register(ProposedXWingMsgs, XwingRegister) ->
    #xwing_register{xwing_messages_info =
                        lists:map(
                          fun(Msg) ->
                                  case lists:keyfind(Msg, 1, XwingRegister#xwing_register.xwing_messages_info) of
                                      false        -> {Msg, 1};
                                      {Msg, Count} -> {Msg, Count + 1}
                                  end
                          end, ProposedXWingMsgs)}.

%% get_xwing_possibility_messages(XwingRegister)
%% Return currently valid xwing_possibility -messages.

-spec get_xwing_possibility_messages(XwingRegister) -> [Msg] when
    XwingRegister ::#xwing_register{},
    Msg           ::tuple().

get_xwing_possibility_messages(XwingRegister) ->
    lists:filtermap(
      fun({Msg, Count}) ->
              case Count =< ?MAX_MSG_COUNT of
                  true  -> {true, Msg};
                  false -> false
              end
      end, XwingRegister#xwing_register.xwing_messages_info).
