let inputToArr =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(item => Js.String2.split(item, ""))

let countTrees = (trees, (right, down)) => {
  trees
  ->Belt.Array.keepWithIndex((_, i) => mod(i, down) === 0)
  ->Belt.Array.mapWithIndex((i, item) => {
    let arrLength = Belt.Array.length(item)
    let right = right * i
    let rightIndex = switch right > arrLength - 1 {
    | true => mod(right, arrLength)
    | false => right
    }
    item[rightIndex]
  })
  ->Belt.Array.keep(item => item === "#")
  ->Belt.Array.length
}

let part1Slope = (3, 1)
let part1Result = inputToArr->countTrees(part1Slope)->Js.log
// 232

let part2Slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
let part2Result =
  part2Slopes
  ->Belt.Array.map(slope => inputToArr->countTrees(slope))
  ->Belt.Array.reduce(1.0, (accumulator, item) => accumulator *. Belt.Float.fromInt(item))
  ->Js.log
// 3952291680
