module Day10

open System.IO

type Machine =
  { indicators: int array
    buttons: int array
    joltages: int array }

let sumOfFewestButtonPresses (machines: Machine array) = 0

let parse filename = filename |> File.ReadAllLines

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day10/test.txt", 7)>]
  [<InlineData("Inputs/Day10/input.txt", -1)>]
  let ``The fewest button presses required to configure the indicator lights`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day10/test.txt", -1)>]
  [<InlineData("Inputs/Day10/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
