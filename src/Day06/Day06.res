let inputToArr =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n\n")
  ->Belt.Array.map(item => Js.String2.split(item, "\n"))

let part1 =
  inputToArr
  ->Belt.Array.map(arr => {
    arr
    ->Belt.Array.reduce("", (acc, item) => acc ++ item)
    ->Js.String2.split("")
    ->Belt.Set.String.fromArray
    ->Belt.Set.String.size
  })
  ->Belt.Array.reduce(0, (acc, item) => acc + item)
  ->Js.log
// 6542

let mergeArrToSet = arr => arr->Belt.Array.concatMany->Belt.Set.String.fromArray

let part2 =
  inputToArr
  ->Belt.Array.map(item => item->Belt.Array.map(line => Js.String2.split(line, "")))
  ->Belt.Array.map(item => {
    item->Belt.Array.reduce(item->mergeArrToSet, (acc, currentItem) =>
      Belt.Set.String.intersect(acc, Belt.Set.String.fromArray(currentItem))
    )
  })
  ->Belt.Array.map(item => Belt.Set.String.toArray(item))
  ->Belt.Array.reduce(0, (acc, item) => acc + Belt.Array.length(item))
  ->Js.log
//3299

// ->Belt.Array.keep(li => {
//   let arrToList = Belt.List.fromArray(li)
//   arrToList->Belt.List.every(item => {
//     Js.String2.length(Belt.List.headExn(arrToList)) === Js.String2.length(item)
//   })
// })
