let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

exception Failed_to_parse_input

let testInput = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."->Js.String2.split("\n")

// {
//   name: 'light red',
//   quantity: 0,
//   innerList: [
//     { name: 'bright white', quantity: 1, innerList: [] },
//     { name: 'muted yellow', quantity: 2, innerList: [] }
//   ]
// }

module Bag = {
  type rec t = {
    name: string,
    quantity: int,
    innerList: array<t>,
  }

  let hasBag = (bag: t, targetName) => bag.name === targetName ? Some(bag) : None

  let rec checkInnerList = (innerList, list, targetName) => {
    innerList
    ->Belt.Array.keepMap(item => {
      switch hasBag(item, targetName) {
      | Some(_) => Some(true)
      | None => {
          let root = list->Belt.Array.getBy(bag => bag.name === item.name)
          switch root {
          | Some(rootItem) => Some(rootItem.innerList->checkInnerList(list, targetName))
          | None => None
          }
        }
      }
    })
    ->Belt.Array.some(v => v)
  }

  let find = (list, ~name) => {
    list->Belt.Array.keepMap(item => {
      item.innerList->checkInnerList(list, name) ? Some(true) : None
    })
  }

  let count = list => list->Belt.Array.length
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
            | Some(v) => {
                let obj = {
                  name: Utils.Array.getLastExn(v),
                  quantity: Belt.Array.getExn(v, 1)->Garter.Int.fromStringExn,
                  innerList: [],
                }
                Some(obj)
              }
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

let part1Name = "shiny gold"

let part1 = parsedInput->Bag.find(~name=part1Name)->Bag.count->Js.log
// 169

let part2 = parsedInput->Js.log
