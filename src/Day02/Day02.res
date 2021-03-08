let inputToArr =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(item => Js.String2.split(item, " "))

let getFirstInArr = arr => Belt.Array.getExn(arr, 0)

let getLastInArr = arr => Belt.Array.getExn(arr, Belt.Array.length(arr) - 1)

let getMinMax = li => {
  let min = getFirstInArr(li)->Belt.Int.fromString->Belt.Option.getWithDefault(0)
  let max = getLastInArr(li)->Belt.Int.fromString->Belt.Option.getWithDefault(0)
  (min, max)
}

let getChar = li => getFirstInArr(li)->Js.String2.replace(":", "")

let getBody = li => getFirstInArr(li)->Js.String2.split("")

let parsePassword = li => {
  let (min, max) = getFirstInArr(li)->getMinMax
  let char = Belt.Array.getExn(li, 1)->getChar
  let textArr = getLastInArr(li)->getBody
  (min, max, char, textArr)
}

let part1 =
  inputToArr
  ->Belt.Array.map(line => {
    line
    ->Belt.Array.map(item => Js.String2.split(item, "-"))
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
