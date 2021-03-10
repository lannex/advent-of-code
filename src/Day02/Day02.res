let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")

exception Fail_to_read_input

type passwordType = (int, int, string, string)

type parseInputType = array<passwordType>

let parseInput = (input: string): parseInputType => {
  input
  ->Js.String2.split("\n")
  ->Belt.Array.map(line => {
    let [_, start, end, char, password] =
      line->Js.String2.splitByReAtMost(%re("/(\d+)-(\d+) (\w): (\w+)/g"), ~limit=5)
    (
      start->Belt.Option.getExn->Garter.Int.fromStringExn,
      end->Belt.Option.getExn->Garter.Int.fromStringExn,
      char->Belt.Option.getExn,
      password->Belt.Option.getExn,
    )
  })
}

let countArr = ls => ls->Belt.Array.keep(item => item)->Belt.Array.length

// let parsePassword: array<string> => option<string>
let parsePassword = (char, password) => {
  password->Js.String2.split("")
}

let part1 =
  inputFromFile
  ->parseInput
  ->Belt.Array.map(((start, end, char, password): passwordType) => {
    let count =
      password->Js.String2.split("")->Belt.Array.keep(item => char == item)->Belt.Array.length
    count >= start && count <= end
  })
  ->countArr
  ->Js.log
// 546

let matchArr = (~start, ~end, ~pwArr, ~char) => {
  let firstMatch = char === Belt.Array.getExn(pwArr, start - 1)
  let secondMatch = char === Belt.Array.getExn(pwArr, end - 1)
  firstMatch !== secondMatch
}

let part2 =
  inputFromFile
  ->parseInput
  ->Belt.Array.map(((start, end, char, password): passwordType) => {
    let pwArr = password->Js.String2.split("")
    pwArr
    ->Belt.Array.map(_ => matchArr(~start, ~end, ~char, ~pwArr))
    ->Utils.Array.getFirst
    ->Belt.Option.getWithDefault(false)
  })
  ->countArr
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
