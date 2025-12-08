module Day8

open Common.Vector3d
open System.Collections.Generic

type JunctionBox = Vector3d
type Circuit = HashSet<JunctionBox>

let findCircuits (junctionBoxes: JunctionBox array) : Circuit array =
  let mutable circuits = List<Circuit>()

  circuits.ToArray()

let multiply3LargestCircuits junctionBoxes =
  junctionBoxes
  |> findCircuits
  |> Array.map _.Count
  |> Array.sortDescending
  |> Array.take 3
  |> Array.reduce (*)

let parse filename : JunctionBox array =
  filename
  |> System.IO.File.ReadAllLines
  |> Array.map (fun s -> s.Split(",") |> Array.map int)
  |> Array.map (fun [| x; y; z |] -> x, y, z)

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", 40)>]
  [<InlineData("Inputs/Day8/input.txt", -1)>]
  let ``Multiply together the sizes of the three largest circuits`` (filename: string, expected: int) =
    let result = filename |> parse |> multiply3LargestCircuits
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", -1)>]
  [<InlineData("Inputs/Day8/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
