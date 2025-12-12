module Day11

open System.Collections.Generic
open System.IO
open Common.Functools

let countPathsToGoal (start: string) (goal: string) (serverRack: IDictionary<string, string[]>) : int64 =
  let rec countPaths memoized (current: string) =
    if current = goal then
      1L
    else
      let mutable hasNeighbors, neighbors = serverRack.TryGetValue current

      seq {
        if hasNeighbors then
          for neighbor in neighbors do
            yield memoized neighbor
      }
      |> Seq.sum

  memoizeRec countPaths start

let parse filename =
  let parseLine (line: string) =
    let s = line.Split(':')
    let key = s[0]
    let value = s[1].Trim().Split(' ')
    key, value

  filename |> File.ReadAllLines |> Array.map parseLine |> dict

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day11/test.txt", 5)>]
  [<InlineData("Inputs/Day11/input.txt", 599)>]
  let ``Part 1: Number of paths leading you out`` (filename: string, expected: int64) =
    let result = filename |> parse |> countPathsToGoal "you" "out"
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day11/test2.txt", 2)>]
  [<InlineData("Inputs/Day11/input.txt", 393474305030400L)>]
  let ``Part 2: Number of paths from svr to out`` (filename: string, expected: int64) =
    let result =
      filename
      |> parse
      |> (fun serverRack ->
        (countPathsToGoal "svr" "fft" serverRack
         * countPathsToGoal "fft" "dac" serverRack
         * countPathsToGoal "dac" "out" serverRack)
        + (countPathsToGoal "svr" "dac" serverRack
           * countPathsToGoal "dac" "fft" serverRack
           * countPathsToGoal "fft" "out" serverRack))

    Assert.Equal(expected, result)
