# AOC 2025

This is a template project for solving Advent Of Code (2025) in Haskell: fork it and you can start solving!


## Organization of the codebase

`src/` contains all the Haskell code. `src/Day01.hs`, `src/Day02.hs`, ... each contain the solution
for that specific day, and they are all imported into the `src/Main.hs`, which is the executable
that we use to easily run the solution for the specific day.

`data/` dir is where puzzle input files go for each day.
Each day, you should save a new file there, named `dayXY-input.txt` where `XY` is day number (`01`, `25`, ...).
Your code in `src/DayXX.hs` can then read those files as needed.
These data files are gitignored, since AoC authors ask not to publicly post your puzzle input files.

## Running

To run the day one solution first you must download the input file provided by *aoc* into the `data/` directory, and name the file `day01-input.txt`.
Then simply run the command:
```
cabal run aoc2025 -- "01"
```

`cabal repl` is also interesting option: it will take you to ghci where all the modules are already
loaded for you and you can easily run any function, this is great for debugging and testing, or even
normal running. You can just type `day01` and run it that way. Type `:r` to reload upon changes in
the code.

## Project config

This is a cabal-based project.

`aoc2025.cabal` file already contains the default configuration that should work fine for you.

You will notice it also contains some commented-out extensions and dependencies(packages). These are
some common extensions and dependencies that we think you are likely to need so we put them there
for your convenience, you just need to uncomment them in order to get going with them.
