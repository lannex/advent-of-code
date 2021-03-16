let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

module Report = {
  let getMatchingItem = (list, n, item) => {
    switch list->Belt.Set.Int.fromArray->Belt.Set.Int.get(n) {
    | Some(_) => Some(item)
    | None => None
    }
  }

  let part1 = (list: array<int>, target) => {
    list->Belt.Array.map(item => list->getMatchingItem(target - item, item))
  }

  let part2 = (list: array<int>, target) => {
    list->Belt.Array.map(item => {
      list
      ->Belt.Array.map(subItem => list->getMatchingItem(target - item - subItem, item))
      ->Belt.Array.getBy(subItem => {
        switch subItem {
        | Some(_) => true
        | None => false
        }
      })
      ->Belt.Option.getWithDefault(None)
    })
  }

  let multiply = list => list->Belt.Array.reduce(1, (acc, cur) => acc * cur)
}

module Parse = {
  let input = line => {
    switch line->Utils.Re.captures(%re("/\d+/")) {
    | Some(value) => {
        let [v] = value
        Some(v->Garter.Int.fromStringExn)
      }
    | None => None
    }
  }
}

let targetYear = 2020

let reportList = inputFromFile->Belt.Array.keepMap(Parse.input)

let part1 =
  reportList->Report.part1(targetYear)->Belt.Array.keepMap(v => v)->Report.multiply->Js.log

let part2 =
  reportList->Report.part2(targetYear)->Belt.Array.keepMap(v => v)->Report.multiply->Js.log
