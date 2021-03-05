let inputToArr =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(item => Js.String2.split(item, ""))

type treesType = {
  xIndex: int,
  trees: float,
}

let countTrees = (arr, (right, down)) => {
  let result =
    arr
    ->Belt.Array.keepWithIndex((_, i) => {
      mod(i, down) === 0
    })
    ->Belt.Array.reduce(({xIndex: -right, trees: 0.0}: treesType), (accumulator, item) => {
      let updatedXIndex = accumulator.xIndex + right
      switch item[mod(updatedXIndex, Belt.Array.length(item))] === "#" {
      | true => {
          xIndex: accumulator.xIndex + right,
          trees: accumulator.trees +. 1.0,
        }
      | false => {
          xIndex: accumulator.xIndex + right,
          trees: accumulator.trees,
        }
      }
    })
  result.trees
}

let part1Slope = (3, 1)
let part1Result = inputToArr->countTrees(part1Slope)

Js.log(part1Result)
// 232

let part2Slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
let part2Result =
  part2Slopes
  ->Belt.Array.map(slope => inputToArr->countTrees(slope))
  ->Belt.Array.reduce(1.0, (accumulator, item) => accumulator *. item)

Js.log(part2Result)
// 3952291680
