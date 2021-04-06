let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

let test = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"->Js.String2.split("\n")

exception Invalid_argument

type seatT =
  | Empty
  | Occupied
  | Floor

type seatsT = array<seatT>

type listT = array<seatsT>

module Parse = {
  let input = raw =>
    raw->Belt.Array.map(line =>
      line
      ->Js.String2.split("")
      ->Belt.Array.map(char =>
        switch char {
        | "L" => Empty
        | "." => Floor
        | _ => raise(Invalid_argument)
        }
      )
    )
}

module Coordinate = {
  type t = {
    x: int,
    y: int,
  }

  let directions = [
    // {x: 0, y: 0}

    // t
    {x: 0, y: 1},
    // tr
    {x: 1, y: 1},
    // r
    {x: 1, y: 0},
    // br
    {x: 1, y: -1},
    // b
    {x: 0, y: -1},
    // bl
    {x: -1, y: -1},
    // l
    {x: -1, y: 0},
    // tl
    {x: -1, y: 1},
  ]

  let setDirections = direction => [
    // {x: direction.x, y: direction.y},
    {x: direction.x, y: direction.y + 1},
    {x: direction.x + 1, y: direction.y + 1},
    {x: direction.x + 1, y: direction.y},
    {x: direction.x + 1, y: direction.y - 1},
    {x: direction.x, y: direction.y - 1},
    {x: direction.x - 1, y: direction.y - 1},
    {x: direction.x - 1, y: direction.y},
    {x: direction.x - 1, y: direction.y + 1},
  ]
}

let parsedList = Parse.input(inputFromFile)

module Count = {
  type checkT = {
    empty: int => seatT,
    occupied: int => seatT,
  }

  let check = {
    empty: count => count === 0 ? Occupied : Empty,
    occupied: count => count >= 4 ? Empty : Occupied,
  }

  let occupiedCount = (list, positions: array<Coordinate.t>) =>
    positions
    ->Belt.Array.map(pos =>
      switch list->Belt.Array.get(pos.x) {
      | Some(seats) =>
        switch seats->Belt.Array.get(pos.y) {
        | Some(seat) => seat === Occupied
        | _ => false
        }
      | None => false
      }
    )
    ->Belt.Array.keep(x => x)
    ->Belt.Array.size

  let occupied = (list, directions) => list->occupiedCount(directions->Coordinate.setDirections)

  let result = list =>
    list->Belt.Array.reduce(0, (sum, row) =>
      sum + row->Belt.Array.keep(seat => seat === Occupied)->Belt.Array.size
    )
}

module Seats = {
  type t = (listT, listT)

  let stopCompare = (isSame, cur, index) => !isSame || cur->Belt.Array.size === index + 1

  let rec doCompareList = (list, index): bool => {
    let (cur, next) = list
    let isSame = cur[index] === next[index]

    stopCompare(isSame, cur, index) ? isSame : list->doCompareList(index + 1)
  }

  let rec doCompare = (lists, index): bool => {
    let (cur, next) = lists
    let isSame = (cur[index], next[index])->doCompareList(0)

    stopCompare(isSame, cur, index) ? isSame : lists->doCompare(index + 1)
  }

  let setNextList = (list, checkCount: Count.checkT) =>
    list->Belt.Array.mapWithIndex((xIndex, seats) =>
      seats->Belt.Array.mapWithIndex((yIndex, item) => {
        let count = list->Count.occupied({x: xIndex, y: yIndex})

        switch item {
        | Floor => Floor
        | Empty => checkCount.empty(count)
        | Occupied => checkCount.occupied(count)
        }
      })
    )

  let rec findNoSeatsChange = (list, checkCount) => {
    let lists = (list, list->setNextList(checkCount))
    let (_, nextList) = lists

    lists->doCompare(0) ? nextList : nextList->findNoSeatsChange(checkCount)
  }
}

let part1 = parsedList->Seats.findNoSeatsChange(Count.check)->Count.result->Js.log
