module Day12

open System.IO
open System.Text.RegularExpressions

type Present = int

type Region =
  { width: int
    height: int
    requiredPresents: int array }

type Input =
  { presents: Present array
    regions: Region array }

let countRegionsWhereRequiredPresentsFit (input: Input) : int = 0

let parse filename : Input =
  let regionsRegex = Regex(@"(?<region>\d+x\d+: [\d *]+)")
  let input = filename |> File.ReadAllText

  let regions =
    regionsRegex.Matches(input)
    |> Seq.map (fun m ->
      let split = m.Value.Split(":")
      let size = split[0].Split("x") |> Array.map int

      { width = size[0]
        height = size[1]
        requiredPresents = split[1].Trim().Split(" ") |> Array.map int })
    |> Seq.toArray

  { presents = [||]; regions = regions }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day12/test.txt", 2)>]
  [<InlineData("Inputs/Day12/input.txt", -1)>]
  let ``Part 1: Number of regions which can fit all of the presents`` (filename: string, expected: int) =
    let result = filename |> parse |> countRegionsWhereRequiredPresentsFit
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day12/test.txt", -1)>]
  [<InlineData("Inputs/Day12/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
