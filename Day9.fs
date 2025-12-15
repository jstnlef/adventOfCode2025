module Day9

open System.IO
open SkiaSharp

let width (x1, _) (x2, _) = (abs x2 - x1) + 1
let height (_, y1) (_, y2) = (abs y2 - y1) + 1

let computeArea (a, b) : int64 =
  (int64 (width a b)) * (int64 (height a b))

let findLargestAreaRectangle (corners: (int * int) array) : int64 =
  (corners, corners) ||> Array.allPairs |> Array.map computeArea |> Array.max

let findLargestAreaRectangleWithRedGreenTiles (corners: (int * int) array) : int64 =
  let path = new SKPath()
  let sx, sy = corners[0]
  path.MoveTo(float32 sx, float32 sy)

  for x, y in corners[1..] do
    path.LineTo(float32 x, float32 y)

  let inArea ((x1, y1), (x2, y2)) =
    let left = min x1 x2 |> float32
    let right = max x1 x2 |> float32
    let bottom = min y1 y2 |> float32
    let top = max y1 y2 |> float32

    [| for x in left..right do
         for y in bottom..top do
           yield x, y |]
    |> Array.Parallel.forall (fun (x, y) -> path.Bounds.Contains(x, y))

  (corners, corners)
  ||> Array.allPairs
  |> Array.filter inArea
  |> Array.map computeArea
  |> Array.max

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
  [<InlineData("Inputs/Day9/input.txt", -1)>]
  let ``Part 2: Find then largest area of any rectangle using only red and green tiles``
    (filename: string, expected: int64)
    =
    let result = filename |> parse |> findLargestAreaRectangleWithRedGreenTiles
    Assert.Equal(expected, result)
