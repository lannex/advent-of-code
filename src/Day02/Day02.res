let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

type passwordInfo = (int, int, string, string)

type password = string

module Password = {
  type t = (int, int, string, string)

  let matchOldPolicy = item => {
    let (start, end, char, password) = item
    let count =
      password->Js.String2.split("")->Belt.Array.keep(item => char == item)->Belt.Array.length
    count >= start && count <= end ? Some(password) : None
  }

  let matchNewPolicy = ((start, end, char, password)) => {
    let pwArr = password->Js.String2.split("")
    pwArr
    ->Belt.Array.keepMap(_ => {
      let firstMatch = char === Belt.Array.getExn(pwArr, start - 1)
      let secondMatch = char === Belt.Array.getExn(pwArr, end - 1)
      firstMatch !== secondMatch ? Some((start, end, password, char)) : None
    })
    ->Utils.Array.getFirst
  }

  let countValid = ls => ls->Belt.Array.length
}

module Parse = {
  let input = (rawLine: string) => {
    let re = %re("/(\d+)-(\d+) (\w): (\w+)/g")
    switch rawLine->Utils.Re.captures(re) {
    | Some(arr) =>
      switch arr {
      | [_, start, end, char, password] =>
        Some((start->Garter.Int.fromStringExn, end->Garter.Int.fromStringExn, char, password))
      | _ => None
      }
    | None => None
    }
  }
}

let passwordList = inputFromFile->Belt.Array.keepMap(Parse.input)

let part1 = passwordList->Belt.Array.keepMap(Password.matchOldPolicy)->Password.countValid->Js.log
// 546

let part2 = passwordList->Belt.Array.keepMap(Password.matchNewPolicy)->Password.countValid->Js.log
// 275
