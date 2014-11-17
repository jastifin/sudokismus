Sudokismus
==========

Sudoku solver in Erlang.

The application tries to solve a given 9x9 sudoku problem where the 9 regions (zones) can be
either the standard 3x3 squares or more irregular shapes. See [Data Format](#dataformat) for more details.

# Build

rebar compile

# Data format

Each row of the 9x9 grid is separated by newline and in each row the cells are separated by semicolons. Individual cells are
coded as zone-x:numbers, where zone-x is the index (1-9) of the region and numbers is the comma-separated list of numbers in that cell. 
The special value '0'  is shortcut for '1,2,3,4,5,6,7,8,9'. See [sample1.txt](./sample.txt) as a example.

# Usage

1. erl -pz ebin
2. application:start(sudoku).
3. sudoku_app:set_values({file, "initialvalues.txt"}).
4. ...wait a while...
5. sudoku_app:dump().

# License

See [LICENSE](./LICENSE).

