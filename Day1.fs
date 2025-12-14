module Day1

open System.IO
open Common.Math

module Rotations =
  let dialTotal = 100

  let findPassword rotations =
    let spinDial (dial, zeroCount) rotation =
      let distance = dial + rotation

      let newZeros =
        abs distance / dialTotal + (if dial <> 0 && distance <= 0 then 1 else 0)

      modulo distance dialTotal, zeroCount + newZeros

    rotations |> Seq.fold spinDial (50, 0) |> snd

  let countZeros rotations =
    rotations
    |> Seq.scan (fun p n -> modulo (p + n) dialTotal) 50
    |> Seq.filter (fun n -> n = 0)
    |> Seq.length

  let parse filename =
    filename
    |> File.ReadLines
    |> Seq.map (fun line ->
      let value = int line[1..]
      if line[0] = 'R' then value else -value)

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", 3)>]
  [<InlineData("Inputs/Day1/input.txt", 1120)>]
  let ``Part 1: Count the number of times the dial is left pointing at 0 after`` (filename: string, expected: int) =
    let result = filename |> Rotations.parse |> Rotations.countZeros
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day1/test.txt", 6)>]
  [<InlineData("Inputs/Day1/input.txt", 6554)>]
  let ``Part 2: The password to open the door using 0x434C49434B method`` (filename: string, expected: int) =
    let result = filename |> Rotations.parse |> Rotations.findPassword
    Assert.Equal(expected, result)
