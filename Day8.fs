module Day8

type Position = int * int * int

let find3LargestCircuits junctionBoxes = 0

let parse filename =
  filename
  |> System.IO.File.ReadAllLines
  |> Array.map (fun s -> s.Split(",") |> Array.map int)

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", 40)>]
  [<InlineData("Inputs/Day8/input.txt", -1)>]
  let ``Multiply together the sizes of the three largest circuits`` (filename: string, expected: int) =
    let result = filename |> parse |> find3LargestCircuits
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", -1)>]
  [<InlineData("Inputs/Day8/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
