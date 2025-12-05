module Day5

open System.IO

type IngredientDatabase =
  { freshRanges: (int64 * int64) array
    ingredientIds: int64 array }

module IngredientDatabase =
  let isFresh (database: IngredientDatabase) (id: int64) =
    database.freshRanges |> Array.exists (fun (a, b) -> id >= a && id <= b)

let countFreshIngredients (database: IngredientDatabase) =
  database.ingredientIds
  |> Array.Parallel.filter (IngredientDatabase.isFresh database)
  |> _.Length

let parse filename =
  let split = filename |> File.ReadAllText |> _.Split("\n\n")

  let ranges =
    split[0].Trim().Split("\n")
    |> Array.map _.Split("-")
    |> Array.map (fun [| a; b |] -> (int64 a, int64 b))

  let ids = split[1].Trim().Split("\n") |> Array.map int64

  { freshRanges = ranges
    ingredientIds = ids }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day5/test.txt", 3)>]
  [<InlineData("Inputs/Day5/input.txt", 623)>]
  let ``The number of fresh ingredient IDs`` (filename: string, expected: int) =
    let result = filename |> parse |> countFreshIngredients
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day5/test.txt", -1)>]
  [<InlineData("Inputs/Day5/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
