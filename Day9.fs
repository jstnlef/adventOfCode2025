module Day9

open System.IO

let findLargestAreaRectangle (corners: (int64 * int64) array) : int64 =
  let computeArea ((x1, y1), (x2, y2)) =
    (abs (x2 - x1 + 1L)) * (abs (y2 - y1 + 1L))

  (corners, corners) ||> Array.allPairs |> Array.map computeArea |> Array.max

let parse filename =
  filename
  |> File.ReadAllLines
  |> Array.map (fun l ->
    let s = l.Split(",")
    int64 s[0], int64 s[1])

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", 50)>]
  [<InlineData("Inputs/Day9/input.txt", 4759930955L)>]
  let ``Find the largest area of any rectangle`` (filename: string, expected: int64) =
    let result = filename |> parse |> findLargestAreaRectangle
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day9/test.txt", 24)>]
  [<InlineData("Inputs/Day9/input.txt", -1)>]
  let ``Find then largest area of any rectangle using only red and green tiles`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
