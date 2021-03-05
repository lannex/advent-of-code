let inputToArr =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(item => Js.String2.split(item, ""))

type rowRangeType = {
  start: int,
  end: int,
}

let getRange = (rows, ~indexRange, ~rowRange) => {
  let (startIndex, endIndex) = indexRange
  rows
  ->Belt.Array.slice(~offset=startIndex, ~len=endIndex)
  ->Belt.Array.reduce((rowRange: rowRangeType), (accumulator, item) => {
    let {start, end} = accumulator
    let center = Js.Math.ceil_int(Belt.Int.toFloat(end - start) /. 2.0)

    switch item {
    | "F" | "L" => {
        start: start,
        end: end - center,
      }
    | _ => {
        start: start + center,
        end: end,
      }
    }
  })
}

let part1Result =
  inputToArr
  ->Belt.Array.map(seats => {
    let row = seats->getRange(~indexRange=(0, 7), ~rowRange={start: 0, end: 127})
    let col = seats->getRange(~indexRange=(7, 3), ~rowRange={start: 0, end: 7})
    row.start * 8 + col.start
  })
  ->Js.Math.maxMany_int

Js.log(part1Result)

// let part2Result;
