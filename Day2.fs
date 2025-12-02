module Day2

open System.IO

let isInvalid (id: string) =
  let mid = id.Length / 2
  id.Substring(0, mid) = id.Substring(mid)

let isInvalidNewRules (id: string) =
  let doubled = id + id
  let inner = doubled.Substring(1, doubled.Length - 2)
  inner.Contains id

let findInvalidIDs (idIsInvalid: string -> bool) idRange =
  idRange
  |> Array.Parallel.collect (fun (first, last) ->
    [| for n in first..last do
         if idIsInvalid (string n) then
           yield n |])

let parse filename =
  filename
  |> File.ReadAllText
  |> _.Split(",")
  |> Array.map (fun s ->
    let split = s.Split("-")
    (int64 split[0], int64 split[1]))

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", 1227775554)>]
  [<InlineData("Inputs/Day2/input.txt", 53420042388L)>]
  let ``The sum of all invalid IDs`` (filename: string, expected: int64) =
    let result = filename |> parse |> findInvalidIDs isInvalid |> Array.sum
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", 4174379265L)>]
  [<InlineData("Inputs/Day2/input.txt", 69553832684L)>]
  let ``The sum of all invalid IDs with the new rules`` (filename: string, expected: int64) =
    let result = filename |> parse |> findInvalidIDs isInvalidNewRules |> Array.sum
    Assert.Equal(expected, result)
