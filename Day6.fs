module Day6

open System
open System.IO
open System.Collections.Generic

type MathOperator =
  | Addition
  | Multiplication

module MathOperator =
  let isOperator (c: char) = c = '+' || c = '*'

  let fromString (c: string) =
    match c with
    | "+" -> Addition
    | "*" -> Multiplication
    | _ -> failwith "Unsupported operation"

  let fromChar (c: char) =
    match c with
    | '+' -> Addition
    | '*' -> Multiplication
    | _ -> failwith "Unsupported operation"

type MathExpression =
  { operator: MathOperator
    operands: int64 array }

module MathExpression =
  let perform (expression: MathExpression) =
    match expression.operator with
    | Addition -> expression.operands |> Array.sum
    | Multiplication -> expression.operands |> Array.reduce (*)

let sumOfMathExpressions expressions =
  expressions |> Array.sumBy MathExpression.perform

let parseAsHuman filename =
  let parsed =
    filename
    |> File.ReadAllLines
    |> Array.map _.Split(" ", StringSplitOptions.RemoveEmptyEntries)

  [| for column in parsed[0].Length - 1 .. -1 .. 0 do
       let row = [| for row in 0 .. parsed.Length - 1 -> parsed[row][column] |]

       let operator = MathOperator.fromString row[row.Length - 1]
       let operands = row |> Array.take (row.Length - 1) |> Array.map int64

       yield
         { operator = operator
           operands = operands } |]

let parseAsCephalopod filename : MathExpression array =
  let parsed = filename |> File.ReadAllLines
  let maxCols = parsed |> Array.map _.Length |> Array.max
  let expressions = List<MathExpression>()
  let mutable operands = List<int64>()

  for column in maxCols - 1 .. -1 .. 0 do
    let chars = List<char>()
    let mutable operator = None

    for row in 0 .. parsed.Length do
      if row = parsed.Length then
        if chars.Count > 0 then
          operands.Add(String(chars.ToArray()) |> int64)

        if operator.IsSome then
          expressions.Add(
            { operator = operator.Value
              operands = operands.ToArray() }
          )

          operands.Clear()

      elif column < parsed[row].Length then
        let c = parsed[row][column]

        if c |> MathOperator.isOperator then
          operator <- Some(c |> MathOperator.fromChar)
        elif c <> ' ' then
          chars.Add(c)

  expressions.ToArray()

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day6/test.txt", 4277556)>]
  [<InlineData("Inputs/Day6/input.txt", 4805473544166L)>]
  let ``The sum of all math problems`` (filename: string, expected: int64) =
    let result = filename |> parseAsHuman |> sumOfMathExpressions
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day6/test.txt", 3263827)>]
  [<InlineData("Inputs/Day6/input.txt", 8907730960817L)>]
  let ``The sum of all math problems with cephalopod math`` (filename: string, expected: int64) =
    let result = filename |> parseAsCephalopod |> sumOfMathExpressions
    Assert.Equal(expected, result)
