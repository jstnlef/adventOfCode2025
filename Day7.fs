module Day7

open System.Collections.Generic

type TachyonManifold =
  { start: int * int
    diagram: char array array }

let countTachyonSplits (input: TachyonManifold) =
  let mutable splits = 0
  let frontier = Queue<int * int>([ input.start ])
  let seen = HashSet<int * int>()

  while frontier.Count > 0 do
    let r, c = frontier.Dequeue()

    if seen.Contains(r, c) |> not then
      seen.Add(r, c) |> ignore
      let nr = r + 1

      if (nr < input.diagram.Length && c >= 0 && c < input.diagram[nr].Length) then
        if input.diagram[nr][c] = '^' then
          splits <- splits + 1
          frontier.Enqueue(nr, c - 1)
          frontier.Enqueue(nr, c + 1)
        else
          frontier.Enqueue(nr, c)

  splits


let parse filename : TachyonManifold =
  let diagram = System.IO.File.ReadAllLines filename |> Array.map _.ToCharArray()
  let startColumn = diagram[0] |> Array.findIndex (fun c -> c = 'S')

  { start = (0, startColumn)
    diagram = diagram }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day7/test.txt", 21)>]
  [<InlineData("Inputs/Day7/input.txt", 1560)>]
  let ``The number of times the tachyon beam splits`` (filename: string, expected: int) =
    let result = filename |> parse |> countTachyonSplits
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day7/test.txt", -1)>]
  [<InlineData("Inputs/Day7/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
