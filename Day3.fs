module Day3

open System.IO

let findMaxBatteryIndex (batteries: string) =
  batteries |> Seq.maxBy int |> (fun i -> batteries.IndexOf(i.ToString()))

let findMaxBatteryJoltage numBatteries (batteries: string) =
  [| let mutable leftMostIndex = 0
     let mutable toFind = numBatteries

     while toFind > 1 do
       let windowLength = batteries.Length - leftMostIndex - toFind + 1
       let batteryWindow = batteries.Substring(leftMostIndex, windowLength)
       let nextBatteryIndex = leftMostIndex + findMaxBatteryIndex batteryWindow
       yield batteries[nextBatteryIndex]

       toFind <- toFind - 1
       leftMostIndex <- nextBatteryIndex + 1

     let lastBatteryIndex =
       leftMostIndex + findMaxBatteryIndex (batteries.Substring(leftMostIndex))

     yield batteries[lastBatteryIndex] |]
  |> System.String
  |> int64

let findTotalMaxBatteryJoltages numBatteries (banks: string[]) =
  banks |> Array.Parallel.sumBy (findMaxBatteryJoltage numBatteries)

let parse filename = filename |> File.ReadAllLines

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", 357)>]
  [<InlineData("Inputs/Day3/input.txt", 17554)>]
  let ``Find the maximum 2 battery output joltage`` (filename: string, expected: int64) =
    let result = filename |> parse |> findTotalMaxBatteryJoltages 2
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", 3121910778619L)>]
  [<InlineData("Inputs/Day3/input.txt", 175053592950232L)>]
  let ``Find the maximum 12 battery output joltage`` (filename: string, expected: int64) =
    let result = filename |> parse |> findTotalMaxBatteryJoltages 12
    Assert.Equal(expected, result)
