module Day2

open System.IO
open MathNet.Numerics

type IdRange = { min: string; max: string }

module IdRange =
  let isValid (id: int64) =
    let s = string id

    if s.Length.IsOdd() then
      false
    else
      let mid = s.Length / 2
      s.Substring(0, mid) = s.Substring(mid)

  let invalidIds (id: IdRange) =
    seq { int64 id.min .. int64 id.max } |> Seq.filter isValid |> Seq.toArray

let findInvalidIDs (ids: IdRange array) = ids |> Array.collect IdRange.invalidIds

let parse filename =
  filename
  |> File.ReadAllText
  |> _.Split(",")
  |> Array.map (fun s ->
    { min = s.Split("-")[0]
      max = s.Split("-")[1] })

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", 1227775554)>]
  [<InlineData("Inputs/Day2/input.txt", 53420042388L)>]
  let ``The sum of all invalid IDs`` (filename: string, expected: int64) =
    let result = filename |> parse |> findInvalidIDs |> Array.sum
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", -1)>]
  [<InlineData("Inputs/Day2/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
