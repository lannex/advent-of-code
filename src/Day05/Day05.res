let inputToArr =
  Node.Fs.readFileAsUtf8Sync("./input.txt")
  ->Js.String2.split("\n")
  ->Belt.Array.map(item => Js.String2.split(item, ""))

type rangeType = {
  start: int,
  end: int,
}

let getRange = (rows, ~indexRange, ~range) => {
  let (startIndex, endIndex) = indexRange
  let (start, end) = range
  rows
  ->Belt.Array.slice(~offset=startIndex, ~len=endIndex)
  ->Belt.Array.reduce(({start: start, end: end}: rangeType), (accumulator, item) => {
    let {start, end} = accumulator
    let center = Js.Math.ceil_int(Belt.Int.toFloat(end - start) /. 2.0)

    switch item {
    | "F" | "L" => {
        start: start,
        end: end - center,
      }
    | "B" | "R" => {
        start: start + center,
        end: end,
      }
    | _ => raise(Not_found)
    }
  })
}

type rangeListType = {
  indexRange: (int, int),
  range: (int, int),
}

let rowRangeList: rangeListType = {
  indexRange: (0, 7),
  range: (0, 127),
}

let colRangeList: rangeListType = {
  indexRange: (7, 3),
  range: (0, 7),
}

let makeSeatId = ((row, col)) => row.start * 8 + col.start

let seatsToRowCol: array<string> => (rangeType, rangeType) = seats => {
  let row = seats->getRange(~indexRange=rowRangeList.indexRange, ~range=rowRangeList.range)
  let col = seats->getRange(~indexRange=colRangeList.indexRange, ~range=colRangeList.range)
  (row, col)
}

let part1Result =
  inputToArr->Belt.Array.map(seatsToRowCol)->Belt.Array.map(makeSeatId)->Js.Math.maxMany_int->Js.log
// 938
