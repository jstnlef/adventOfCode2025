module Day1

open System.IO
open Common.Math

module Rotations =
  let countZeros rotations =
    rotations
    |> Seq.scan (fun state n -> modulo (state + n) 100) 50
    |> Seq.filter (fun n -> n = 0)
    |> Seq.length

  let parse filename =
    filename
    |> File.ReadLines
    |> Seq.map (fun line -> if line[0] = 'R' then int line[1..] else -(int line[1..]))

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", 3)>]
  [<InlineData("Inputs/Day1/input.txt", 1120)>]
  let ``Count the number of times the dial is left pointing at 0 after`` (filename: string, expected: int) =
    let result = filename |> Rotations.parse |> Rotations.countZeros
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", -1)>]
  [<InlineData("Inputs/Day1/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
