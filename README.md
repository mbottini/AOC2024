# AOC 2024

## Mike Bottini

This is an F# implementation of Advent of Code 2024.

# Building

    dotnet build

# Running

Most programs are built to run with the following format:

    ./prog -p[1|2] <input_filepath>

If you call the program `input_filepath` set to `-`, it will instead take input
from standard input.

The easiest way to do this is with `dotnet` itself, as follows:

    dotnet run --project <Day> -- -p[1|2] <path/to/input/file.txt>

As an example:

    $ dotnet run --project Day01 -- -p1 - <<< \
        "3 4
        4 3
        2 5
        1 3
        3 9
        3 3"
    11