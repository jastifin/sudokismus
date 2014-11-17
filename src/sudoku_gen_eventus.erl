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
%% @doc Sudoku gen_server

-module(sudoku_gen_eventus).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_event).

-include("sudoku_gen_eventus.hrl").

-export([start/1, init/1,
         handle_event/2,
         handle_call/2,
         terminate/2,
         format_status/2,
         code_change/3,
         handle_info/2]).

-spec start({{row | col | cell, atom()}}) -> {ok, pid()} | {error, term()}.
start({{CellType, Id}}) ->
    case gen_event:start_link({local, Id}) of
        {ok, Pid} ->
            case gen_event:add_handler(Id, sudoku_gen_eventus, {{CellType, Id}}) of
                ok -> {ok, Pid};
                {'EXIT', Reason} -> {error, Reason}
            end;
        {error, Msg} ->
            {error, Msg}
    end.

-spec init({{row | col | cell, atom()}}) -> {ok, #cell_state{}}.

init({{CellType, Id}}) ->
    {ok, #cell_state{type = CellType, id = Id}}.

handle_event(_, State) when State#cell_state.is_done == true ->
    {ok, State};

handle_event(Event, State) ->
    NewValuesTmp = sudoku_event_handlerismus:do_event_handling(Event, State),
    validate_values(State#cell_state.values, NewValuesTmp, Event),
    NewValues    = sudoku_algoritmus:perform_cleanup(NewValuesTmp),
    NewMsgs      = sudoku_messagismus:generate_msgs(NewValues, State#cell_state.type, State#cell_state.id),
    check_duplicates(NewMsgs),

    Msgs = NewMsgs -- State#cell_state.sent_msgs,
    check_duplicates(Msgs),
    check_duplicates(State#cell_state.sent_msgs ++ Msgs),
    sudoku_messagismus:send_others(State#cell_state.id, Msgs),

    IsDone = sudoku_boardismus:is_done(NewValues),
    case IsDone == true andalso State#cell_state.type == row of
        true -> io:format("~p is ready~n", [State#cell_state.id]);
        false -> ok
    end,

    {ok, State#cell_state{values    = NewValues,
                          sent_msgs = State#cell_state.sent_msgs ++ Msgs,
                          is_done   = IsDone
                         }}.

handle_call({init, Values}, State) ->
    {ok, ok, reset_state(State, Values)};

handle_call({dump}, State) ->
    {ok, State#cell_state.values, State};

handle_call({dump_state}, State) ->
    {ok, State, State};

handle_call(Request, State) ->
    io:format("~p: ~p~n",[State, Request]),
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_Oldvsn, State, _Extra) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

format_status(_Opt, _List) ->
    ok.

-spec check_duplicates(List) -> ok | no_return() when
    List ::[any()].
check_duplicates(List) ->
    lists:foldl(fun(E, AccIn) ->
            case lists:member(E, AccIn) of true -> io:format("--------> DUPLICATE ~p ~n",[E]), erlang:error(duplicate); false -> [E | AccIn] end
        end, [], List),
    ok.

validate_values(OldValues, Values, _Event) ->
    lists:foreach(
      fun({_, _, _, Numbers}) ->
              L = length(Numbers),
              case L == 0 of
                  true  -> io:format("~p ~p ~n", [OldValues, Values]),
                           ?assert(L > 0);
                  false -> ok
              end
      end, Values).

reset_state(State, Values) ->
    State#cell_state{values = Values, sent_msgs = [], is_done = false}.


%% print(Event, State, Senders, Receivers)  ->
%%     case length(Senders) == 0 orelse lists:member(element(2, Event), Senders) of
%%         true -> case length(Receivers) == 0 orelse lists:member(State#cell_state.id, Receivers) of
%%                     true  -> io:format("~p received: ~p --->~n~p ~p~n", [State#cell_state.id, Event, State#cell_state.values, State#cell_state.sent_msgs]), ok;
%%                     false -> ignored
%%                 end;
%%         false -> ignored
%%     end.
%% print_test() ->
%%     Event = {row, row4, has, {"Msg"}},
%%     State = #cell_state{id = row6, values="Values", sent_msgs="Sent"},
%%     ?assertEqual(print(Event, State, [row4], [row6]), ok).