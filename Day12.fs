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

let countRegionsWhereRequiredPresentsFit (input: Input) : int =
  input.regions
  |> Seq.filter (fun region ->
    region.requiredPresents
    |> Array.mapi (fun i n -> input.presents[i] * n)
    |> Array.sum < region.width * region.height)
  |> Seq.length

let presentsRegex = Regex(@"(?<presentIndex>\d+):\n([#\.]+\n)+")
let regionsRegex = Regex(@"(?<region>\d+x\d+: [\d *]+)")

let parse filename : Input =
  let input = filename |> File.ReadAllText

  let presents =
    presentsRegex.Matches(input)
    |> Seq.map (fun m -> m.Value |> Seq.sumBy (fun c -> if c = '#' then 1 else 0))
    |> Seq.toArray

  let regions =
    regionsRegex.Matches(input)
    |> Seq.map (fun m ->
      let split = m.Value.Split(":")
      let size = split[0].Split("x") |> Array.map int

      { width = size[0]
        height = size[1]
        requiredPresents = split[1].Trim().Split(" ") |> Array.map int })
    |> Seq.toArray

  { presents = presents
    regions = regions }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day12/test.txt", 3)>]
  [<InlineData("Inputs/Day12/input.txt", 512)>]
  let ``Part 1: Number of regions which can fit all of the presents`` (filename: string, expected: int) =
    let result = filename |> parse |> countRegionsWhereRequiredPresentsFit
    Assert.Equal(expected, result)
