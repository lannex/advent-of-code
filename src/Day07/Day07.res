let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

module Bag = {
  type rec t = {
    name: string,
    quantity: int,
    innerList: array<t>,
  }

  let hasBag = (item: t, targetName): option<t> => item.name === targetName ? Some(item) : None

  let getBag = (list, name): option<t> => list->Belt.Array.getBy(item => item.name === name)

  let rec checkNameInnerList = (innerList, list, targetName) => {
    innerList
    ->Belt.Array.keepMap(item => {
      switch hasBag(item, targetName) {
      | Some(_) => Some(true)
      | None =>
        switch getBag(list, item.name) {
        | Some(rootItem) => Some(rootItem.innerList->checkNameInnerList(list, targetName))
        | None => None
        }
      }
    })
    ->Belt.Array.some(v => v)
  }

  let checkName = (list, ~name) => {
    list->Belt.Array.keepMap(item => {
      item.innerList->checkNameInnerList(list, name) ? Some(true) : None
    })
  }

  let rec countName = (list, ~name) =>
    switch getBag(list, name) {
    | Some(selectedItem) =>
      selectedItem.innerList
      ->Belt.Array.map(item => {
        list->countName(~name=item.name) * item.quantity
      })
      ->Belt.Array.reduce(1, (acc, cur) => acc + cur)
    | None => 0
    }
}

module Parse = {
  open Bag

  let input = line => {
    let re =
      line
      ->Js.String2.replaceByRe(%re("/( ?)(bag)(s?)(\.)?/g"), "")
      ->Js.String2.replaceByRe(%re("/(no other)/g"), "0 no")
      ->Utils.Re.captures(%re("/^(.*)( contain )(.*)/"))

    switch re {
    | Some(arr) => {
        let outer = Belt.Array.getExn(arr, 1)
        let inner =
          Utils.Array.getLastExn(arr)
          ->Js.String2.split(",")
          ->Belt.Array.keepMap(item => {
            let capturedItem = item->Utils.Re.captures(%re("/(\d+) (.*)/"))
            switch capturedItem {
            | Some(v) =>
              Some({
                name: Utils.Array.getLastExn(v),
                quantity: Belt.Array.getExn(v, 1)->Garter.Int.fromStringExn,
                innerList: [],
              })
            | None => None
            }
          })
        Some({
          name: outer,
          quantity: 1,
          innerList: inner,
        })
      }
    | None => None
    }
  }
}

let parsedInput = inputFromFile->Belt.Array.keepMap(Parse.input)

let name = "shiny gold"

let countPart1 = list => Belt.Array.length(list)
let part1 = parsedInput->Bag.checkName(~name)->countPart1->Js.log
// 169

let countPart2 = count => count - 1
let part2 = parsedInput->Bag.countName(~name)->countPart2->Js.log
// 82372
