let inputToArr =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(item => Js.String2.split(item, ""))

let countTrees = (~right, ~down, arr) => {
  let trees = ref(0)
  let xIndex = ref(-right)
  let yIndex = ref(down)

  arr
  ->Js.Array2.filteri((_, i) => {
    mod(i, down) === 0
  })
  ->Belt.Array.forEach(item => {
    xIndex := xIndex.contents + right

    switch item[mod(xIndex.contents, Belt.Array.length(item))] === "#" {
    | true => trees := trees.contents + 1
    | false => ()
    }

    yIndex := yIndex.contents + down
  })
  trees.contents
}

let part1Result = countTrees(~right=3, ~down=1, inputToArr)
Js.log(part1Result)
// 232

let part2Slopes = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
part2Slopes->Belt.Array.forEach(slope => {
  let test = countTrees(~right=slope[0], ~down=slope[1], inputToArr)
  Js.log(test)
  // [ 86, 232, 90, 71, 31 ]
})
