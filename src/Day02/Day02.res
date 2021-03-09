let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")

exception Fail_to_read_input

let parseInputToArr = (input: string) => {
  input
  ->Js.String2.split("\n")
  ->Belt.Array.map(line => {
    line
    ->Js.String2.splitByReAtMost(%re("/(\d+)-(\d+) (\w): (\w+)/g"), ~limit=5)
    ->Belt.Array.map(item => {
      switch item {
      | Some(value) => value
      | None => raise(Fail_to_read_input)
      }
    })
    ->Belt.Array.keep(item => item !== "")
  })
}

module Password = {
  type policy =
    | PART1
    | PART2

  type passwordType = {
    start: int,
    end: int,
    char: string,
    pwArr: array<string>,
  }

  let getItems = (~lineArr) => {
    let start = Utils.Array.getFirst(lineArr)->Belt.Int.fromString->Belt.Option.getWithDefault(0)
    let end = Belt.Array.getExn(lineArr, 1)->Belt.Int.fromString->Belt.Option.getWithDefault(0)
    let char = Belt.Array.getExn(lineArr, 2)
    let pwArr = Utils.Array.getLast(lineArr)->Js.String2.split("")
    {
      start: start,
      end: end,
      char: char,
      pwArr: pwArr,
    }
  }

  let compareArr = (~char, ~pwArr) => {
    let newArr = Belt.Array.make(Belt.Array.length(pwArr), char)
    Belt.Array.keepWithIndex(newArr, (newArrItem, i) =>
      newArrItem === Belt.Array.getExn(pwArr, i)
    )->Belt.Array.length
  }

  let matchArr = (~start, ~end, ~pwArr, ~char) => {
    let firstMatch = char === Belt.Array.getExn(pwArr, start - 1)
    let secondMatch = char === Belt.Array.getExn(pwArr, end - 1)
    firstMatch !== secondMatch
  }

  let validPassword = (~lineArr, ~policy) => {
    let {start, end, char, pwArr}: passwordType = getItems(~lineArr)

    switch policy {
    | PART1 =>
      pwArr
      ->Belt.Array.map(_ => compareArr(~char, ~pwArr))
      ->Utils.Array.getFirst
      ->(count => count >= start && count <= end)
    | PART2 =>
      pwArr->Belt.Array.map(_ => matchArr(~start, ~end, ~char, ~pwArr))->Utils.Array.getFirst
    }
  }
}

open Password

let part1 =
  inputFromFile
  ->parseInputToArr
  ->Belt.Array.map(lineArr => validPassword(~lineArr, ~policy=PART1))
  ->Belt.Array.keep(item => item)
  ->Belt.Array.length
  ->Js.log
// 546

let part2 =
  inputFromFile
  ->parseInputToArr
  ->Belt.Array.map(lineArr => validPassword(~lineArr, ~policy=PART2))
  ->Belt.Array.keep(item => item)
  ->Belt.Array.length
  ->Js.log
// 275
