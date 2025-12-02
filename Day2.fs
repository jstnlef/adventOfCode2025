module Day2

open System.IO

let isValidOld (id: string) =
  let mid = id.Length / 2
  id.Substring(0, mid) = id.Substring(mid)

let isValidNew (id: string) =
  let doubled = id + id
  let inner = doubled.Substring(1, doubled.Length - 2)
  inner.Contains id

let findInvalidIDs (validFunc: string -> bool) idRange =
  idRange
  |> Seq.collect (fun (min, max) -> seq { int64 min .. int64 max } |> Seq.filter (fun n -> validFunc (string n)))

let parse filename =
  filename
  |> File.ReadAllText
  |> _.Split(",")
  |> Seq.map (fun s ->
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
