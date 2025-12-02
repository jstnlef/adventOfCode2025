module Day2

open System.IO

let isValidOld (id: int64) =
  let s = string id
  let mid = s.Length / 2
  s.Substring(0, mid) = s.Substring(mid)

let isValidNew (id: int64) =
  let isRepeatedSubstring (s: string) =
    let doubled = s + s
    let inner = doubled.Substring(1, doubled.Length - 2)
    inner.Contains s

  id |> string |> isRepeatedSubstring

let findInvalidIDs (validFunc: int64 -> bool) idRange =
  idRange
  |> Seq.collect (fun (min, max) -> seq { int64 min .. int64 max } |> Seq.filter validFunc)

let parse filename =
  filename
  |> File.ReadAllText
  |> _.Split(",")
  |> Array.map (fun s ->
    let split = s.Split("-")
    (split[0], split[1]))

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", 1227775554)>]
  [<InlineData("Inputs/Day2/input.txt", 53420042388L)>]
  let ``The sum of all invalid IDs`` (filename: string, expected: int64) =
    let result = filename |> parse |> findInvalidIDs isValidOld |> Seq.sum
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", 4174379265L)>]
  [<InlineData("Inputs/Day2/input.txt", 69553832684L)>]
  let ``The sum of all invalid IDs with the new rules`` (filename: string, expected: int64) =
    let result = filename |> parse |> findInvalidIDs isValidNew |> Seq.sum
    Assert.Equal(expected, result)
