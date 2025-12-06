module Day6

open System.IO

let doMathProblems (problems: string[][]) =
  let mutable sum = 0L

  for i in 0 .. problems[0].Length - 1 do
    let op = problems[problems.Length - 1][i]

    let args =
      [| for j in 0 .. problems.Length - 2 do
           int64 (problems[j][i]) |]

    sum <-
      sum
      + match op with
        | "+" -> args |> Array.sum
        | "*" -> args |> Array.fold (*) 1L
        | _ -> failwith "Invalid operation"

  sum

let parseAsHuman filename =
  filename
  |> File.ReadAllLines
  |> Array.map (fun line -> line.Split(" ") |> Array.filter (fun x -> x <> ""))

let parseAsCephalopod filename = [||]

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day6/test.txt", 4277556)>]
  [<InlineData("Inputs/Day6/input.txt", 4805473544166L)>]
  let ``The sum of all math problems`` (filename: string, expected: int64) =
    let result = filename |> parseAsHuman |> doMathProblems
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day6/test.txt", 3263827)>]
  [<InlineData("Inputs/Day6/input.txt", -1)>]
  let ``The sum of all math problems with cephalopod math`` (filename: string, expected: int64) =
    let result = filename |> parseAsCephalopod |> doMathProblems
    Assert.Equal(expected, result)
