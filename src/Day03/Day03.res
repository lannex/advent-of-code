let inputToArr =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(item => Js.String2.split(item, ""))

type treesType = {
  xIndex: int,
  trees: float,
}

let countTrees = (~right, ~down, arr) => {
  arr
  ->Belt.Array.keepWithIndex((_, i) => {
    mod(i, down) === 0
  })
  ->Belt.Array.reduce({xIndex: -right, trees: 0.0}, (accumulator, item) => {
    let updatedXIndex = accumulator.xIndex + right
    if item[mod(updatedXIndex, Belt.Array.length(item))] === "#" {
      {
        xIndex: accumulator.xIndex + right,
        trees: accumulator.trees +. 1.0,
      }
    } else {
      {
        xIndex: accumulator.xIndex + right,
        trees: accumulator.trees,
      }
    }
  })
}

let {trees: part1Result} = countTrees(~right=3, ~down=1, inputToArr)
Js.log(part1Result)
// 232

let part2Slopes = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
// part2Slopes->Belt.Array.forEach(slope => {
//   let test = countTrees(~right=slope[0], ~down=slope[1], inputToArr)
//   Js.log(test)
//   // [ 86, 232, 90, 71, 31 ]
// })


let part2Result = part2Slopes->Belt.Array.reduce(1.0, (accumulator, slope) => {
  let {trees: result} = countTrees(~right=slope[0], ~down=slope[1], inputToArr)
  Js.log(accumulator)
  accumulator *. result
})

Js.log(part2Result)
// 3952291680
