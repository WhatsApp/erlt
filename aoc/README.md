A section to put solutions to advent of code 2020.

In order to join: 
* choose a name 'your_entry'
* create a directory 'your_entry' under aoc/
* add the 'your_entry' to the list of apps dirs in aoc/rebar.config
* add a src directory under aoc/your_entry
* copy pergu/src/pergu.app.src to that directory and rename it to your_entry.app.src
* change the first line in that file to '{application, your_entry, [' instead of '{application, pergu, ['
* Put your source code in aoc/your_entry/src and compile with rebar3 compile 