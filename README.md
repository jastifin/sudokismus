Sudokismus
==========

Sudoku solver in Erlang.

# Build

rebar compile

# Usage

1. erl -pz ebin
2. application:start(sudoku).
3. sudoku_app:set_values({file, "initialvalues.txt"}).
4. ...wait a while...
5. sudoku_app:dump().

# License

See LICENSE.txt

