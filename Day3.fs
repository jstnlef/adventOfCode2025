module Day3

open System.IO

let findMaxIndex (batteries: string) =
  seq { 9..-1..1 }
  |> Seq.find (fun i -> batteries.IndexOf(i.ToString()) > -1)
  |> (fun max -> batteries.IndexOf(max.ToString()))

let findMax2BatteryJoltage (batteries: string) =
  let leftMaxIndex = findMaxIndex (batteries.Substring(0, batteries.Length - 1))

  let rightMaxIndex =
    findMaxIndex (batteries.Substring(leftMaxIndex + 1)) + leftMaxIndex + 1

  int (System.String [| batteries[leftMaxIndex]; batteries[rightMaxIndex] |])

let findMaxBatteryJoltage numBatteries (batteries: string) =
  [| let mutable leftMostI = 0
     let mutable found = 0

     while found < numBatteries do
       let subBattery = batteries.Substring(leftMostI, batteries.Length - numBatteries)

       let maxIndex = findMaxIndex subBattery
       found <- found + 1
       yield batteries[leftMostI + maxIndex]
       leftMostI <- leftMostI + maxIndex + 1 |]
  |> System.String
  |> int64

let findTotalMax2BatteryJoltages (banks: string[]) =
  banks |> Array.sumBy findMax2BatteryJoltage

let findTotalMaxBatteryJoltages numBatteries (banks: string[]) =
  banks |> Array.sumBy (findMaxBatteryJoltage numBatteries)

let parse filename = filename |> File.ReadAllLines

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", 357)>]
  [<InlineData("Inputs/Day3/input.txt", 17554)>]
  let ``Find the maximum 2 battery output joltage`` (filename: string, expected: int) =
    let result = filename |> parse |> findTotalMax2BatteryJoltages
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day3/test.txt", 3121910778619L)>]
  [<InlineData("Inputs/Day3/input.txt", -1)>]
  let ``Find the maximum 12 battery output joltage`` (filename: string, expected: int64) =
    let result = filename |> parse |> findTotalMaxBatteryJoltages 12
    Assert.Equal(expected, result)
