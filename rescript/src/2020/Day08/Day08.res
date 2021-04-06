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

  let rec doRun = (stopFn, nextFn, cur) => {
    if stopFn(cur) {
      nextFn(cur)
    } else {
      doRun(stopFn, nextFn, nextFn(cur))
    }
  }

  let run = (list, initState) => {
    let stop = s => s.job !== Loop
    let setNext = cur => setNextProgramState(cur, list->Belt.Array.get(cur.index))
    doRun(stop, setNext, initState)
  }

  let swapOperation = code =>
    switch code {
    | Nop(value) => Jmp(value)
    | Jmp(value) => Nop(value)
    | _ => code
    }

  let runSwap = (list, initState, ~index) => {
    let setNextState = j => {
      list
      ->Belt.Array.mapWithIndex((i, item) => i === j ? swapOperation(item) : item)
      ->run(initState)
    }

    let setNext = ((_, currentIndex)) => {
      let nextIndex = currentIndex - 1
      let nextState = setNextState(nextIndex)
      (nextState, nextIndex)
    }

    let stop = ((state, i)) => {
      let (next, _) = setNext((state, i))
      next.job !== Infinite
    }

    let (result, _) = doRun(stop, setNext, (setNextState(index), index))
    result
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

let initState: Program.stateT = {index: 0, total: 0, job: Loop, indexSets: Belt.Set.Int.empty}

let instructionList = inputFromFile->Belt.Array.keepMap(Parser.input)

// uncurrying
// let part1Program = instructionList->Program.run(initState)

// currying
let run = Program.run(instructionList)
let part1Program = run(initState)

let part1 = part1Program.total->Js.log
// 1317

// uncurrying
// let part2Program =
//   instructionList->Program.runSwap(initState, ~index=Belt.Array.length(inputFromFile))

// currying
let swap = Program.runSwap(instructionList)
let part2Program = swap(initState, ~index=Belt.Array.length(inputFromFile))

let part2 = part2Program.total->Js.log
// 1033
