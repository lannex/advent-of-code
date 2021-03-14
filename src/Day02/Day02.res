let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

module Password = {
  type t = (int, int, string, string)

  let validateOldPolicy = item => {
    switch item {
    | Some((start, end, char, password)) => {
        let count =
          password->Js.String2.split("")->Belt.Array.keep(item => char == item)->Belt.Array.length
        Some(count >= start && count <= end)
      }
    | None => None
    }
  }

  let matchArr = (~start, ~end, ~pwArr, ~char) => {
    let firstMatch = char === Belt.Array.getExn(pwArr, start - 1)
    let secondMatch = char === Belt.Array.getExn(pwArr, end - 1)
    firstMatch !== secondMatch
  }

  let validateNewPolicy = item => {
    switch item {
    | Some((start, end, char, password)) => {
        let pwArr = password->Js.String2.split("")
        Some(
          pwArr
          ->Belt.Array.map(_ => matchArr(~start, ~end, ~char, ~pwArr))
          ->Utils.Array.getFirst
          ->Belt.Option.getWithDefault(false),
        )
      }
    | None => None
    }
  }

  let countValid = ls => ls->Belt.Array.keep(item => item)->Belt.Array.length
}

module Parse = {
  let input = (rawLine: string) => {
    let re = %re("/(\d+)-(\d+) (\w): (\w+)/g")
    switch rawLine->Utils.Re.captures(re) {
    | Some(arr) => {
        let [_, start, end, char, password] = arr
        Some((start->Garter.Int.fromStringExn, end->Garter.Int.fromStringExn, char, password))
      }
    | None => None
    }
  }
}

let part1 =
  inputFromFile
  ->Belt.Array.map(Parse.input)
  ->Belt.Array.keepMap(Password.validateOldPolicy)
  ->Password.countValid
  ->Js.log
// 546

let part2 =
  inputFromFile
  ->Belt.Array.map(Parse.input)
  ->Belt.Array.keepMap(Password.validateNewPolicy)
  ->Password.countValid
  ->Js.log
// 275
