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

// type numbersToIntT = {
//   list: list<int>,
//   currentTarget: int,
// }

// type numbersToFloat = {
//   list: list<float>,
//   currentTarget: float,
// }

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

  let findSubList = (subList, nextFn, target) => {
    let stopSubList = ((l, v, _)) => Belt.Option.isSome(v) || Belt.List.length(l) === 0
    let (_, match, currentTarget) = stopSubList->doFind(nextFn, (subList, None, target))
    (match, currentTarget)
  }

  let findInvalidNumber = (arr, ~preamble) => {
    let list = arr->Belt.List.fromArray

    let rec doFindInvalidNumber = (list, ~preamble) => {
      switch list->Belt.List.get(preamble) {
      | Some(target) => {
          let (headList, _) =
            list->Belt.List.splitAt(preamble)->Belt.Option.getWithDefault((list{}, list{}))
          let subList = headList->Belt.List.map(item => target - item)

          let setNextSubList = ((l, _, currentTarget)) => {
            let newSubList = Belt.List.tail(l)->Belt.Option.getWithDefault(list{})
            let newSubListHead = Belt.List.head(newSubList)
            (newSubList, findHeadList(headList, newSubListHead), currentTarget)
          }
          let (match, currentTarget) = subList->findSubList(setNextSubList, target)

          switch match {
          | Some(_) =>
            switch Belt.List.tail(list) {
            | Some(v) => doFindInvalidNumber(v, ~preamble)
            | None => None
            }
          | None => Some(currentTarget)
          }
        }

      | None => raise(Invalid_argument)
      }
    }

    doFindInvalidNumber(list, ~preamble)
  }

  // type encryptionWeaknessT = {
  //   currentIndex: int,
  //   // length: int,
  //   result: array<float>,
  // }

  let rec findEncryptionWeakness = (list, target, state) => {
    list
  }
}

module type ParseT = {
  type t
  let input: string => option<t>
}

module ParseToInt: ParseT with type t = int = {
  type t = int
  let input = (line): option<t> => line->Belt.Int.fromString
}

module ParseToFloat: ParseT with type t = float = {
  type t = float
  let input = (line): option<t> => line->Belt.Float.fromString
}

// input: 25
// test: 5
let preamble = 25

let parsedList = parse => inputFromFile->Belt.Array.keepMap(parse)

let part1 = ParseToInt.input->parsedList->Numbers.findInvalidNumber(~preamble)
Js.log(part1)
// 57195069

let part2 = ParseToFloat.input->parsedList->Numbers.findEncryptionWeakness(part1)->Js.log

//
//
//
