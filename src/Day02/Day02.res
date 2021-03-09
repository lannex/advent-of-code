let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")

exception Fail_to_read_input

let parseInputToArr = (input: string) => {
  input
  ->Js.String2.split("\n")
  ->Belt.Array.map(line => {
    let [_, start, end, char, pwArr] =
      line->Js.String2.splitByReAtMost(%re("/(\d+)-(\d+) (\w): (\w+)/g"), ~limit=5)
    [
      start->Belt.Option.getWithDefault(""),
      end->Belt.Option.getWithDefault(""),
      char->Belt.Option.getWithDefault(""),
      pwArr->Belt.Option.getWithDefault(""),
    ]
  })
}

// let test = parseInputToArr(inputFromFile)->Js.log

module Password = {
  type pwArrType = array<string>

  type passwordType = {
    start: int,
    end: int,
    char: string,
    pwArr: pwArrType,
  }

  let getItems = ls => {
    let start = Utils.Array.getFirst(ls)->Belt.Int.fromString->Belt.Option.getWithDefault(0)
    let end = Belt.Array.getExn(ls, 1)->Belt.Int.fromString->Belt.Option.getWithDefault(0)
    let char = Belt.Array.getExn(ls, 2)
    let pwArr = Utils.Array.getLast(ls)->Js.String2.split("")
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
}

open Password

// parseInt("1") => 1
// parseInt("a") == NaN

// let parsePassword: array<string> => option<string>
// [Some("ccccc"), None, Some("abcde") ...]
// Some length

// let parsePassword: passwordInput => option<password>

// let printPassword: password => unit = p => Js.log(p);

// let parseUser = userInput => option<user>

// let getUsername = user => string = u => u.name;

// [true, false, ]
// to
// [Some("abcde"), None, Some("bbbbb") ...]

let part1 =
  inputFromFile
  ->parseInputToArr
  ->Belt.Array.map(ls =>
    ls
    ->getItems
    ->(
      ({start, end, pwArr, char}) => {
        pwArr
        ->Belt.Array.map(_ => compareArr(~char, ~pwArr))
        ->Utils.Array.getFirst
        ->(count => count >= start && count <= end)
      }
    )
  )
  ->Belt.Array.keep(item => item)
  ->Belt.Array.length
  ->Js.log
// 546

let part2 =
  inputFromFile
  ->parseInputToArr
  ->Belt.Array.map(ls =>
    ls
    ->getItems
    ->(
      ({start, end, pwArr, char}) => {
        pwArr->Belt.Array.map(_ => matchArr(~start, ~end, ~char, ~pwArr))->Utils.Array.getFirst
      }
    )
  )
  ->Belt.Array.keep(item => item)
  ->Belt.Array.length
  ->Js.log
// 275
