module Day1

open System.IO
open Common.Math

module Rotations =
  let findPassword rotations =
    let x =
      rotations |> Array.scan (fun (p, zeros) n -> modulo (p + n) 100, zeros) (50, 0)

    0

  let countZeros rotations =
    rotations
    |> Array.scan (fun state n -> modulo (state + n) 100) 50
    |> Array.filter (fun n -> n = 0)
    |> Array.length

  let parse filename =
    filename
    |> File.ReadLines
    |> Seq.map (fun line -> if line[0] = 'R' then int line[1..] else -(int line[1..]))
    |> Seq.toArray

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", 3)>]
  [<InlineData("Inputs/Day1/input.txt", 1120)>]
  let ``Count the number of times the dial is left pointing at 0 after`` (filename: string, expected: int) =
    let result = filename |> Rotations.parse |> Rotations.countZeros
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", 6)>]
  [<InlineData("Inputs/Day1/input.txt", -1)>]
  let ``What is the password to open the door using 0x434C49434B method`` (filename: string, expected: int) =
    let result = filename |> Rotations.parse |> Rotations.findPassword
    Assert.Equal(expected, result)
