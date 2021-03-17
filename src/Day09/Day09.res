let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

exception Invalid_argument

let test = "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"->Js.String2.split("\n")

// let result =
//   subList
//   ->Belt.List.keepMap(s => {
//     let l = headList->Belt.List.keepMap(h => {
//       s === h ? Some(s) : None
//     })

//     switch Belt.List.length(l) > 0 {
//     | true => Some(l->Belt.List.toArray)
//     | false => None
//     }
//   })
//   ->Belt.List.toArray
//   ->Belt.Array.concatMany
// Js.log(result)

module Numbers = {
  let rec doFind = (stopFn, nextFn, cur) => {
    if stopFn(cur) {
      cur
    } else {
      doFind(stopFn, nextFn, nextFn(cur))
    }
  }

  let findHeadList = (headList, newSubListHead) => {
    let setNextHeadList = ((sl, _)) => {
      let newHeadList = Belt.List.tail(sl)->Belt.Option.getWithDefault(list{})
      let newHeadListHead = Belt.List.head(newHeadList)
      (newHeadList, newHeadListHead)
    }
    let stopHeadList = ((sl, sh)) => sh === newSubListHead || Belt.List.length(sl) === 0
    let (_, match) = stopHeadList->doFind(setNextHeadList, (headList, Belt.List.head(headList)))
    match
  }

  let findSubList = (headList, subList, target) => {
    let setNextSubList = ((l, _, tar)) => {
      let newSubList = Belt.List.tail(l)->Belt.Option.getWithDefault(list{})
      let newSubListHead = Belt.List.head(newSubList)
      let match = findHeadList(headList, newSubListHead)
      (newSubList, match, tar)
    }
    let stopSubList = ((l, v, _)) => Belt.Option.isSome(v) || Belt.List.length(l) === 0
    let (_, match, currentTarget) = stopSubList->doFind(setNextSubList, (subList, None, target))
    (match, currentTarget)
  }

  let rec findError = (list, ~preamble) => {
    switch list->Belt.List.get(preamble) {
    | Some(target) => {
        let (headList, _) =
          list->Belt.List.splitAt(preamble)->Belt.Option.getWithDefault((list{}, list{}))
        let subList = headList->Belt.List.map(item => target - item)

        let (match, currentTarget) = findSubList(headList, subList, target)

        switch match {
        | Some(_) =>
          switch Belt.List.tail(list) {
          | Some(v) => findError(v, ~preamble)
          | None => None
          }
        | None => Some(currentTarget)
        }
      }

    | None => raise(Invalid_argument)
    }
  }
}

module Parse = {
  let input = line => line->Belt.Int.fromString
}

let preamble = 25 // test is 5

let parsedList = inputFromFile->Belt.Array.keepMap(Parse.input)

let part1 = parsedList->Belt.List.fromArray->Numbers.findError(~preamble)->Js.log
// 57195069
