module Day8

open Common
open System.Collections.Generic

type JunctionBox = Vector3d.Vector3d
type Circuit = HashSet<JunctionBox>

let findShortestPairs (junctionBoxes: JunctionBox array) =
  seq {
    for i in 0 .. junctionBoxes.Length - 1 do
      for j in i + 1 .. junctionBoxes.Length - 1 do
        yield junctionBoxes[i], junctionBoxes[j]
  }
  |> Seq.sortBy (fun (a, b) -> Vector3d.distanceSquared a b)

let connectCircuit (circuits: List<Circuit>) (a, b) =
  let findCircuit a =
    circuits |> Seq.tryFind (fun c -> c.Contains a)

  match findCircuit a, findCircuit b with
  | None, None -> circuits.Add(HashSet([| a; b |]))
  | Some c, None -> c.Add b |> ignore
  | None, Some c -> c.Add a |> ignore
  | Some c1, Some c2 ->
    if c1 <> c2 then
      circuits.Remove c2 |> ignore

      for j in c2 do
        c1.Add j |> ignore
    else
      ()

  circuits

let multiplyLargestCircuits numberOfCircuits (junctionBoxes: JunctionBox array) =
  junctionBoxes
  |> findShortestPairs
  |> Seq.take numberOfCircuits
  |> Seq.fold connectCircuit (List<Circuit>())
  |> Seq.map _.Count
  |> Seq.sortDescending
  |> Seq.take 3
  |> Seq.reduce (*)

let multiplyLastTwoJunctionBoxes (junctionBoxes: JunctionBox array) : int64 =
  let circuits = List<Circuit>()

  junctionBoxes
  |> findShortestPairs
  |> Seq.skipWhile (fun pair -> pair |> connectCircuit circuits |> (fun c -> c[0].Count < junctionBoxes.Length))
  |> Seq.head
  |> (fun ((x1, _, _), (x2, _, _)) -> int64 x1 * int64 x2)


let parse filename : JunctionBox array =
  filename
  |> System.IO.File.ReadAllLines
  |> Array.map (fun s -> s.Split(",") |> Array.map int)
  |> Array.map (fun [| x; y; z |] -> x, y, z)

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", 10, 40)>]
  [<InlineData("Inputs/Day8/input.txt", 1000, 121770)>]
  let ``Multiply together the sizes of the three largest circuits``
    (filename: string, numberOfCircuits: int, expected: int)
    =
    let result = filename |> parse |> multiplyLargestCircuits numberOfCircuits
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day8/test.txt", 25272)>]
  [<InlineData("Inputs/Day8/input.txt", 7893123992L)>]
  let ``Multiply together the X coordinates of the last two junction boxes`` (filename: string, expected: int64) =
    let result = filename |> parse |> multiplyLastTwoJunctionBoxes
    Assert.Equal(expected, result)
