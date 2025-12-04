module Day4

open System.IO
open Common.Grid

let paperRollIsAccessible (floor: Grid<char>) location =
  location
  |> eightWayNeighbors floor
  |> Array.filter (fun neighbor -> (get floor neighbor) = '@')
  |> Array.length < 4

let accessiblePaperRolls (floor: Grid<char>) =
  floor
  |> iter
  |> Seq.filter (fun loc -> (get floor loc) = '@' && (paperRollIsAccessible floor loc))
  |> Seq.toArray

let countAccessiblePaperRolls (floor: Grid<char>) =
  floor |> accessiblePaperRolls |> _.Length

let removePaperRolls (floor: Grid<char>) (rolls: (int * int) array) : unit =
  for x, y in rolls do
    floor[y][x] <- '.'

let countRemovedPaperRolls (floor: Grid<char>) =
  let mutable rollsToRemove = accessiblePaperRolls floor
  let mutable rollsRemoved = rollsToRemove.Length

  while rollsToRemove.Length > 0 do
    removePaperRolls floor rollsToRemove
    rollsToRemove <- accessiblePaperRolls floor
    rollsRemoved <- rollsRemoved + rollsToRemove.Length

  rollsRemoved

let parse filename =
  filename |> File.ReadAllLines |> Array.map _.ToCharArray()

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day4/test.txt", 13)>]
  [<InlineData("Inputs/Day4/input.txt", 1416)>]
  let ``The number of accessible paper rolls`` (filename: string, expected: int) =
    let result = filename |> parse |> countAccessiblePaperRolls
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day4/test.txt", 43)>]
  [<InlineData("Inputs/Day4/input.txt", 9086)>]
  let ``The number of paper rolls which can be removed`` (filename: string, expected: int) =
    let result = filename |> parse |> countRemovedPaperRolls
    Assert.Equal(expected, result)
