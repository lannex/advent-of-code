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
