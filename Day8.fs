module Day8

open Common
open System.Collections.Generic

type JunctionBox = Vector3d.Vector3d
type Circuit = HashSet<JunctionBox>

let multiplyLargestCircuits numberOfCircuits (junctionBoxes: JunctionBox array) =
  let shortestDistanceJunctionBoxes =
    seq {
      for i in 0 .. junctionBoxes.Length - 1 do
        for j in i + 1 .. junctionBoxes.Length - 1 do
          yield junctionBoxes[i], junctionBoxes[j]
    }
    |> Seq.sortBy (fun (a, b) -> Vector3d.distanceSquared a b)
    |> Seq.take numberOfCircuits

  0

let parse filename : JunctionBox array =
  filename
  |> System.IO.File.ReadAllLines
  |> Array.map (fun s -> s.Split(",") |> Array.map int)
  |> Array.map (fun [| x; y; z |] -> x, y, z)

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", 10, 40)>]
  [<InlineData("Inputs/Day8/input.txt", 1000, -1)>]
  let ``Multiply together the sizes of the three largest circuits``
    (filename: string, numberOfCircuits: int, expected: int)
    =
    let result = filename |> parse |> multiplyLargestCircuits numberOfCircuits
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", -1)>]
  [<InlineData("Inputs/Day8/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
