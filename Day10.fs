module Day10

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text.RegularExpressions
open Microsoft.Z3

type MachineDescription =
  { indicators: uint16
    indicatorSize: int
    buttons: uint16 array array
    joltages: int array }

type Machine =
  { indicators: uint16
    indicatorSize: int
    buttonPresses: int }

module Machine =
  let initMachine indicatorSize =
    { indicators = 0us
      indicatorSize = indicatorSize
      buttonPresses = 0 }

  let pressConfigButton (machine: Machine) (button: uint16 array) : Machine =
    let button =
      button
      |> Array.fold (fun a n -> a + (1us <<< machine.indicatorSize - 1 - int n)) 0us

    { machine with
        indicators = machine.indicators ^^^ button
        buttonPresses = machine.buttonPresses + 1 }

  let findMinimalInitConfig (description: MachineDescription) =
    let startMachine = initMachine description.indicatorSize
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
    let ctx = new Context()
    let optimiser = ctx.MkOptimize()
    let zero = ctx.MkInt(0)

    let buttons =
      [| for i in 0 .. description.buttons.Length - 1 -> ctx.MkIntConst($"b{i}") |]

    for button in buttons do
      optimiser.Add(ctx.MkGe(button, zero))

    let makeEquation (joltageIndex: uint16) =
      let associatedButtons =
        description.buttons
        |> Array.indexed
        |> Array.filter (fun (_, b) -> b |> Array.contains joltageIndex)
        |> Array.map (fun (i, _) -> buttons[i] :> ArithExpr)

      ctx.MkEq(ctx.MkAdd associatedButtons, ctx.MkInt(description.joltages[int joltageIndex]))

    for joltageIndex in 0 .. description.joltages.Length - 1 do
      optimiser.Add(makeEquation (uint16 joltageIndex))

    let buttonExprs = buttons |> Array.map (fun expr -> expr :> ArithExpr)

    optimiser.MkMinimize(ctx.MkAdd buttonExprs) |> ignore

    if optimiser.Check() = Status.SATISFIABLE then
      let model = optimiser.Model

      let buttonPresses =
        model.Decls
        |> Array.sortBy _.Name.ToString()
        |> Array.map (fun decl -> (model.ConstInterp decl).ToString() |> int)

      buttonPresses |> Array.sum
    else
      failwith "No solution found"

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
      |> Seq.map (fun c -> c.Value.Split(',') |> Array.map uint16)
      |> Seq.toArray

    let joltages =
      m.Groups["joltages"].Captures
      |> Seq.map (fun c -> c.Value.Split(',') |> Array.map int |> Seq.toArray)
      |> Seq.head

    { indicators = indicators
      indicatorSize = indicatorString.Length
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
  [<InlineData("Inputs/Day10/input.txt", 21111)>]
  let ``Part 2: The fewest button presses required to set the joltages`` (filename: string, expected: int64) =
    let result = filename |> parse |> sumOfJoltageConfiguration
    Assert.Equal(expected, result)
