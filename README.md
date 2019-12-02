# advent-of-code-haskell

## Introduction

[Advent of Code](https://adventofcode.com/) (AoC) occurs over 25 days, with two
parts (i.e., problems) given for each day. The executable built in this project,
`aoc-exe`, is meant to make it easier to test solutions to the problems, and for
multiple users.

> Advent of Code is an Advent calendar of small programming puzzles for a
  variety of skill sets and skill levels that can be solved in any programming
  language you like. People use them as a speed contest, interview prep, company
  training, university coursework, practice problems, or to challenge each other.

## Usage

Calling the executable `aoc-exe` with the appropriate arguments and source data
saved should compute the solution to an AoC problem.

```
janey@computer~/advent-of-code-haskell$ stack build
...
janey@computer~/advent-of-code-haskell$ stack exec aoc-exe -- <user> <day> <part>
janey@computer~/advent-of-code-haskell$ <answer>
```

### Arguments

- `<user>`: identifier of one working on problems (currently, Janey or Thomas)
- `<day>`: identifier of the problem day (1 to 25 inclusive)
- `<part>`: identifier of the problem part (1 or 2)

As an example, getting Janey's solution to the Day 1 Part 2 problem would
look like this on the command line: `stack exec aoc-exe -- Janey 1 2`

#### Types n such

The main function in [Main.hs](app/Main.hs) enables the

- capturing of arguments
- parsing of arguments to data structures
- selection of appropriate functions to be applied to output a solution
- handling of errors, malformed arguments, etc

```
-- <user> argument must be readable as one of the data constructors
data User = Janey | Thomas
- <day> argument must be readable as an Int to satisfy this sum type
newtype Day = Day Int
-- <part> argument, if 1 or 2, is case matched to either constructor
data Part = One | Two
```

### Input data

Even if the arguments to running the executable are well formed, there still may
be an error if the input data is not saved, or has an improperly formed name.

Input data should be saved in the [inputs](inputs/) directory, in a directory
named after the user. Each input data file should be named with the format
`day<int>-part<int>`.

### Solution organization

Even if the arguments are well formed and the input data is saved in the proper
location with the proper naming, there still may be an unexpected issue.
Currently, there is a Map structure called `problems` that stores details of
problems completed and available to be used. When the well-formed arguments are
passed to the `main` function, a lookup is done on the Map for a problem with
those identifiers. If the problemID is not found, an error is returned. If the
problemID is found, a function `(Text -> Text)` is returned, and that function
is then applied over the appropriate input data set.

TLDR: to use this program appropriately, one needs to update the `problems`
Map in [Main.hs](app/Main.hs) with each new problem identifier and the function
to be called when that problemID is called.

## Motivation

- fun!
- practice!

## Future work

- automate/facilitate the updating of the `problems` Map
- replace the `problems` map with something else
- track attempts to problems, whether completed/failed, etc

