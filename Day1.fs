module Day1

open System.IO
open Common.Math

module Rotations =
  let findPassword rotations =
    let doRotations (p, zeros) next =
      let delta = p + next

      let newZeros =
        if p = 0 || delta > 0 then
          (abs delta) / 100
        else
          1 + (abs delta) / 100

      modulo delta 100, zeros + newZeros

    rotations
    |> Seq.fold doRotations (50, 0)
    |> snd

  let countZeros rotations =
    rotations
    |> Seq.scan (fun p n -> modulo (p + n) 100) 50
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
  [<InlineData("Inputs/Day1/test.txt", 6)>]
  [<InlineData("Inputs/Day1/input.txt", 6554)>]
  let ``The password to open the door using 0x434C49434B method`` (filename: string, expected: int) =
    let result = filename |> Rotations.parse |> Rotations.findPassword
    Assert.Equal(expected, result)
