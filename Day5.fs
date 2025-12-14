module Day5

open System.IO

type IngredientDatabase =
  { freshRanges: (int64 * int64) array
    ingredientIds: int64 array }

module IngredientDatabase =
  let isFresh (id: int64) (database: IngredientDatabase) =
    database.freshRanges
    |> Array.exists (fun (low, high) -> id >= low && id <= high)

  let countFreshIngredients (database: IngredientDatabase) =
    database.ingredientIds
    |> Array.sumBy (fun id -> if isFresh id database then 1 else 0)

  let countAllPossibleFreshIngredients (database: IngredientDatabase) =
    let inputRanges = database.freshRanges
    let mutable low, high = inputRanges[0]
    let mutable count = 0L

    for nl, nh in inputRanges[1..] do
      if nl > high then
        count <- count + high - low + 1L
        low <- nl
        high <- nh
      else
        high <- max high nh

    count + high - low + 1L

let parse filename =
  let split = filename |> File.ReadAllText |> _.Split("\n\n")

  let ranges =
    split[0].Trim().Split("\n")
    |> Array.map _.Split("-")
    |> Array.map (fun [| a; b |] -> (int64 a, int64 b))

  let ids = split[1].Trim().Split("\n") |> Array.map int64

  { freshRanges = ranges |> Array.sort
    ingredientIds = ids }

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day5/test.txt", 3)>]
  [<InlineData("Inputs/Day5/input.txt", 623)>]
  let ``Part 1: The number of fresh ingredient IDs`` (filename: string, expected: int) =
    let result = filename |> parse |> IngredientDatabase.countFreshIngredients
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day5/test.txt", 14)>]
  [<InlineData("Inputs/Day5/input.txt", 353507173555373L)>]
  let ``Part 2: The total number of ingredient IDs considered fresh`` (filename: string, expected: int64) =
    let result =
      filename |> parse |> IngredientDatabase.countAllPossibleFreshIngredients

    Assert.Equal(expected, result)
