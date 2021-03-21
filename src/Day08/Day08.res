let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

exception Failed_to_parse_input

module Program = {
  type codeT =
    | Acc(int)
    | Jmp(int)
    | Nop(int)

  type jopT =
    | Finish
    | Loop
    | Infinite

  type stateT = {
    index: int,
    total: int,
    job: jopT,
    indexSets: Belt.Set.Int.t,
  }

  let setNextProgramState = (selectedItem, currentItem) => {
    let {index, total} = selectedItem
    if Belt.Set.Int.has(selectedItem.indexSets, index) {
      {
        ...selectedItem,
        job: Infinite,
      }
    } else {
      switch currentItem {
      | Some(Acc(value)) => {
          ...selectedItem,
          index: index + 1,
          total: total + value,
          indexSets: Belt.Set.Int.add(selectedItem.indexSets, index),
        }

      | Some(Jmp(value)) => {
          ...selectedItem,
          index: index + value,
          indexSets: Belt.Set.Int.add(selectedItem.indexSets, index),
        }

      | Some(Nop(_)) => {
          ...selectedItem,
          index: index + 1,
          indexSets: Belt.Set.Int.add(selectedItem.indexSets, index),
        }

      | None => {
          ...selectedItem,
          job: Finish,
        }
      }
    }
  }

  let run = (list, init) => {
    let stop = s => s.job !== Loop

    let rec doRun = (next, cur) => {
      if stop(cur) {
        next(cur)
      } else {
        doRun(next, next(cur))
      }
    }

    let next = cur => setNextProgramState(cur, list->Belt.Array.get(cur.index))
    doRun(next, init)
  }

  let swapOperation = code =>
    switch code {
    | Nop(value) => Jmp(value)
    | Jmp(value) => Nop(value)
    | _ => code
    }

  let runSwap = (list, initData, ~index) => {
    let getNext = j => {
      list
      ->Belt.Array.mapWithIndex((i, item) => i === j ? swapOperation(item) : item)
      ->run(initData)
    }
    let stop = i => getNext(i).job !== Infinite

    let rec doRun = (next, cur) => {
      if stop(cur) {
        next(cur)
      } else {
        doRun(next, cur - 1)
      }
    }

    doRun(getNext, index)
  }
}

module Parser = {
  open Program

  let input = rawLine =>
    switch rawLine->Utils.Re.captures(%re("/(acc|jmp|nop) (\+|\-)(\d+)/")) {
    | Some(result) => {
        let argument =
          (Belt.Array.getExn(result, 2) ++ Utils.Array.getLastExn(result))->Garter.Int.fromStringExn
        let operation = Belt.Array.getExn(result, 1)->(
          op => {
            switch op {
            | "acc" => Acc(argument)
            | "jmp" => Jmp(argument)
            | "nop" => Nop(argument)
            | _ => raise(Failed_to_parse_input) // Can't access this point
            }
          }
        )
        Some(operation)
      }
    | None => None
    }
}

let initData: Program.stateT = {index: 0, total: 0, job: Loop, indexSets: Belt.Set.Int.empty}

let instructionList = inputFromFile->Belt.Array.keepMap(Parser.input)

// let p1Runner = Program.run(instructionList)
// let part1Program = p1Runner(initData)

let part1Program = instructionList->Program.run(initData)

let part1 = part1Program.total->Js.log
// 1317

let part2Program =
  instructionList->Program.runSwap(initData, ~index=Belt.Array.length(inputFromFile))

let part2 = part2Program.total->Js.log
// 1033
