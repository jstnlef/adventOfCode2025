module Day2

module Tests =
  open Xunit

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", -1)>]
  [<InlineData("Inputs/Day2/input.txt", -1)>]
  let ``Part 1`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Inputs/Day2/test.txt", -1)>]
  [<InlineData("Inputs/Day2/input.txt", -1)>]
  let ``Part 2`` (filename: string, expected: int) =
    let result = 0
    Assert.Equal(expected, result)
