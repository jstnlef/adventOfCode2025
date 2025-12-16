module Day9

open System.IO
open SkiaSharp

let findLargestAreaRectangle (corners: (int * int) array) : int64 =
  let computeArea ((x1, y1), (x2, y2)) : int64 =
    (int64 ((abs (x2 - x1)) + 1)) * (int64 ((abs (y2 - y1)) + 1))

  (corners, corners) ||> Array.allPairs |> Array.maxBy computeArea |> computeArea

let findLargestAreaRectangleWithRedGreenTiles (corners: (int * int) array) : int64 =
  use path = new SKPath()
  let sx, sy = corners[0]
  path.MoveTo(float32 sx, float32 sy)

  for x, y in corners[1..] do
    path.LineTo(float32 x, float32 y)

  use region = new SKRegion(path)

  let computeArea (rect: SKRectI) =
    (int64 rect.Width + 1L) * (int64 rect.Height + 1L)

  (corners, corners)
  ||> Array.allPairs
  |> Array.map (fun ((x1, y1), (x2, y2)) -> SKRectI(x1, y1, x2, y2).Standardized)
  |> Array.filter region.Contains
  |> Array.maxBy computeArea
  |> computeArea

let parse filename =
  filename
  |> File.ReadAllLines
  |> Array.map (fun l ->
    let s = l.Split(",")
    int s[0], int s[1])

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", 50)>]
  [<InlineData("Inputs/Day9/input.txt", 4759930955L)>]
  let ``Part 1: Find the largest area of any rectangle`` (filename: string, expected: int64) =
    let result = filename |> parse |> findLargestAreaRectangle
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", 24)>]
  [<InlineData("Inputs/Day9/input.txt", 1525241870)>]
  let ``Part 2: Find then largest area of any rectangle using only red and green tiles``
    (filename: string, expected: int64)
    =
    let result = filename |> parse |> findLargestAreaRectangleWithRedGreenTiles
    Assert.Equal(expected, result)
