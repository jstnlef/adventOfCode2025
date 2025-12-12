module Day11

open System.Collections.Generic
open System.IO

let findAllPathsOut (serverRack: IDictionary<string, string[]>) =
  let start = "you"

  let rec dfs (current: string) (path: string list) (visited: Set<string>) =
    seq {
      if current = "out" then
        yield List.rev path
      else
        let _, neighbors = serverRack.TryGetValue current

        for n in neighbors do
          if not (visited.Contains n) then
            yield! dfs n (n :: path) (visited.Add n)
    }

  dfs start [ start ] (Set [ start ]) |> Seq.length


let findAllPathsFromSvrToOut (serverRack: IDictionary<string, string[]>) =
  let start = "svr"

  let rec dfs (current: string) (path: string list) (visited: Set<string>) =
    seq {
      if current = "out" then
        yield List.rev path
      else
        let _, neighbors = serverRack.TryGetValue current

        for n in neighbors do
          if not (visited.Contains n) then
            yield! dfs n (n :: path) (visited.Add n)
    }

  dfs start [ start ] (Set [ start ])
  |> Seq.filter (fun path -> List.contains "dac" path && List.contains "fft" path)
  |> Seq.length

let parse filename =
  let parseLine (line: string) =
    let s = line.Split(':')
    let key = s[0]
    let value = s[1].Trim().Split(' ')
    key, value

  filename |> File.ReadAllLines |> Array.map parseLine |> dict

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day11/test.txt", 5)>]
  [<InlineData("Inputs/Day11/input.txt", 599)>]
  let ``Part 1: Number of paths leading you out`` (filename: string, expected: int) =
    let result = filename |> parse |> findAllPathsOut
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day11/test2.txt", 2)>]
  [<InlineData("Inputs/Day11/input.txt", -1)>]
  let ``Part 2: Number of paths from svr to out`` (filename: string, expected: int) =
    let result = filename |> parse |> findAllPathsFromSvrToOut
    Assert.Equal(expected, result)
