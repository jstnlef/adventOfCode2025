module Day3

open System.IO

let findMax2BatteryJoltage (batteries: string) =
  let findMaxIndex (batteries: string) =
    seq { 9..-1..1 }
    |> Seq.find (fun i -> batteries.IndexOf(i.ToString()) > -1)
    |> (fun max -> batteries.IndexOf(max.ToString()))

  let leftMaxIndex = findMaxIndex (batteries.Substring(0, batteries.Length - 1))

  let rightMaxIndex =
    findMaxIndex (batteries.Substring(leftMaxIndex + 1)) + leftMaxIndex + 1

  int (System.String [| batteries[leftMaxIndex]; batteries[rightMaxIndex] |])

let findTotalMax2BatteryJoltages (banks: string[]) =
  banks |> Array.sumBy findMax2BatteryJoltage

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
  [<InlineData("Inputs/Day3/test.txt", -1)>]
  [<InlineData("Inputs/Day3/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
