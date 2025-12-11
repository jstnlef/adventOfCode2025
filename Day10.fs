module Day10

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text.RegularExpressions
open MathNet.Numerics.LinearAlgebra

type MachineDescription =
  { indicators: uint16
    buttons: uint16 array
    joltages: int array }

type Machine =
  { indicators: uint16
    buttonPresses: int
    joltages: int array }

module Machine =
  let initMachine joltageLength =
    { indicators = 0us
      buttonPresses = 0
      joltages = [| for _ in 0 .. joltageLength - 1 -> 0 |] }

  let pressConfigButton (machine: Machine) (button: uint16) : Machine =
    { machine with
        indicators = machine.indicators ^^^ button
        buttonPresses = machine.buttonPresses + 1 }

  let findMinimalInitConfig (description: MachineDescription) =
    let startMachine = initMachine description.joltages.Length
    let q = Queue<Machine>([| startMachine |])
    let seenIndictors = HashSet<uint16>([| 0us |])

    let mutable machine = q.Peek()

    while machine.indicators <> description.indicators do
      machine <- q.Dequeue()

      for button in description.buttons do
        let nextMachine = pressConfigButton machine button

        if seenIndictors.Contains(nextMachine.indicators) |> not then
          seenIndictors.Add(nextMachine.indicators) |> ignore
          q.Enqueue(nextMachine)

    machine

  let findMinimalJoltageConfig (description: MachineDescription) =
    let buttons =
      description.buttons
      |> Array.map (fun b ->
        Convert.ToString(int16 b, 2).PadLeft(description.joltages.Length, '0')
        |> Seq.map Char.GetNumericValue
        |> Seq.toList)
      |> Array.toList
      |> matrix

    let expectedJoltages = description.joltages |> Array.map double |> vector
    buttons.Solve(expectedJoltages) |> Vector.map (round >> int) |> Seq.sum

let sumOfInitialization (machineDescriptions: MachineDescription array) =
  machineDescriptions
  |> Array.Parallel.map Machine.findMinimalInitConfig
  |> Array.sumBy _.buttonPresses

let sumOfJoltageConfiguration (machineDescriptions: MachineDescription array) =
  machineDescriptions |> Array.map Machine.findMinimalJoltageConfig |> Array.sum

let parse filename =
  let machineRegex =
    Regex(@"^\[(?<indicators>[#\.]+)\] (\((?<buttons>[\d,]+)\) *)+ {(?<joltages>[\d,]+)}$")

  let parseMachineDescription (line: string) =
    let m = machineRegex.Match(line)

    let indicatorString = m.Groups["indicators"].Value

    let indicators =
      indicatorString
      |> String.map (fun c -> if c = '#' then '1' else '0')
      |> (fun s -> UInt16.Parse(s, NumberStyles.BinaryNumber))

    let buttons =
      m.Groups["buttons"].Captures
      |> Seq.map (fun c ->
        c.Value.Split(',')
        |> Array.map uint16
        |> Array.fold (fun a n -> a + (1us <<< indicatorString.Length - 1 - int n)) 0us)
      |> Seq.toArray

    let joltages =
      m.Groups["joltages"].Captures
      |> Seq.map (fun c -> c.Value.Split(',') |> Array.map int |> Seq.toArray)
      |> Seq.head

    { indicators = indicators
      buttons = buttons
      joltages = joltages }

  filename |> File.ReadAllLines |> Array.map parseMachineDescription

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day10/test.txt", 7)>]
  [<InlineData("Inputs/Day10/input.txt", 547)>]
  let ``Part 1: The fewest button presses required to configure the indicator lights``
    (filename: string, expected: int)
    =
    let result = filename |> parse |> sumOfInitialization
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day10/test.txt", 33)>]
  [<InlineData("Inputs/Day10/input.txt", -1)>]
  let ``Part 2: The fewest button presses required to set the joltages`` (filename: string, expected: int) =
    let result = filename |> parse |> sumOfJoltageConfiguration
    Assert.Equal(expected, result)
