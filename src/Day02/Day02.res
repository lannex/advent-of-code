let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")

exception Fail_to_read_input

let parseInput = (input: string) => {
  input
  ->Js.String2.split("\n")
  ->Belt.Array.map(line => {
    let [_, start, end, char, password] =
      line->Js.String2.splitByReAtMost(%re("/(\d+)-(\d+) (\w): (\w+)/g"), ~limit=5)
    (
      start->Belt.Option.getExn->Belt.Int.fromString->Belt.Option.getExn,
      end->Belt.Option.getExn->Belt.Int.fromString->Belt.Option.getExn,
      char->Belt.Option.getExn,
      password->Belt.Option.getExn,
    )
  })
}

module Password = {
  // type passwordType = (option<int>, option<int>, option<string>, option<string>)
  type passwordType = (int, int, string, string)

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

let part1 =
  inputFromFile
  ->parseInput
  ->Belt.Array.map(ls =>
    ls->(
      ((start, end, char, password): passwordType) => {
        let pwArr = password->Js.String2.split("")
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
  ->parseInput
  ->Belt.Array.map(ls =>
    ls->(
      ((start, end, char, password): passwordType) => {
        let pwArr = password->Js.String2.split("")
        pwArr->Belt.Array.map(_ => matchArr(~start, ~end, ~char, ~pwArr))->Utils.Array.getFirst
      }
    )
  )
  ->Belt.Array.keep(item => item)
  ->Belt.Array.length
  ->Js.log
// 275

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
