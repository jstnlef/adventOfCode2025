module Day7

open System.Collections.Generic
open Common.Functools

type TachyonManifold =
  { start: int * int
    diagram: char array array }

module TachyonManifold =
  let countTachyonSplits (manifold: TachyonManifold) =
    let mutable splits = 0
    let frontier = Queue<int * int>([ manifold.start ])
    let seen = HashSet<int * int>()

    while frontier.Count > 0 do
      let r, c = frontier.Dequeue()

      if seen.Contains(r, c) |> not then
        seen.Add(r, c) |> ignore
        let nr = r + 1

        if (nr < manifold.diagram.Length && c >= 0 && c < manifold.diagram[nr].Length) then
          if manifold.diagram[nr][c] = '^' then
            splits <- splits + 1
            frontier.Enqueue(nr, c - 1)
            frontier.Enqueue(nr, c + 1)
          else
            frontier.Enqueue(nr, c)

    splits

  let countPossibleTimelines (manifold: TachyonManifold) =
    let rec count (memoizedCount: int * int -> int64) (r, c) =
      if r = manifold.diagram.Length then
        1L
      elif manifold.diagram[r][c] = '^' then
        memoizedCount (r, c - 1) + memoizedCount (r, c + 1)
      else
        memoizedCount (r + 1, c)

    memoizeRec count manifold.start

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
    let result = filename |> parse |> TachyonManifold.countTachyonSplits
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day7/test.txt", 40)>]
  [<InlineData("Inputs/Day7/input.txt", 25592971184998L)>]
  let ``The number of different timelines a single tachyon particle could take`` (filename: string, expected: int64) =
    let result = filename |> parse |> TachyonManifold.countPossibleTimelines
    Assert.Equal(expected, result)
