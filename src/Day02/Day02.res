let inputToArr =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(item => Js.String2.split(item, " "))

let getMinMax = li => {
  let range = Belt.List.headExn(li)
  let min = Belt.List.headExn(range)->Belt.Int.fromString->Belt.Option.getWithDefault(0)
  let max =
    Belt.List.tailExn(range)->Belt.List.headExn->Belt.Int.fromString->Belt.Option.getWithDefault(0)
  (min, max)
}

let getChar = li =>
  Belt.List.tailExn(li)->Belt.List.headExn->Belt.List.headExn->Js.String2.replace(":", "")

let getBody = li =>
  Belt.List.tailExn(li)
  ->Belt.List.tailExn
  ->Belt.List.headExn
  ->Belt.List.headExn
  ->Js.String2.split("")

let parsePassword = li => {
  let (min, max) = getMinMax(li)
  let char = getChar(li)
  let textArr = getBody(li)
  (min, max, char, textArr)
}

let part1 =
  inputToArr
  ->Belt.Array.map(line => {
    line
    ->Belt.Array.map(item => Js.String2.split(item, "-")->Belt.List.fromArray)
    ->Belt.List.fromArray
    ->parsePassword
    ->(
      ((min, max, char, textArr)) => {
        textArr
        ->Belt.Array.map(_ => {
          let newArr = Belt.Array.make(Belt.Array.length(textArr), char)
          let compareArr = Belt.Array.keepWithIndex(newArr, (newArrItem, i) => {
            newArrItem === Belt.Array.getExn(textArr, i)
          })->Belt.Array.length
          compareArr
        })
        ->Belt.Array.getExn(0)
        ->(count => count >= min && count <= max ? 1 : 0)
      }
    )
  })
  ->Belt.Array.reduce(0, (acc, item) => acc + item)
  ->Js.log
// 546
