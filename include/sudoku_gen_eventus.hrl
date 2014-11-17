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
-record(cell_state, {
    type      = undefined::atom(),
    id        = undefined::atom(),
    values    = []::list({integer(),integer(),integer(),list(integer())}),
    sent_msgs = []::list(tuple()),
    is_done   = false::boolean()
    }).

-define(SET_MAX_SIZE, 5).