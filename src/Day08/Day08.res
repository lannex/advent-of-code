let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

exception Failed_to_parse_input

exception Failed_to_loop

module Program = {
  type codeT =
    | Acc(int)
    | Jmp(int)
    | Nop(int)

  type jopT =
    | Finish
    | Loop
    | Infinite

  type programStateT = {
    index: int,
    total: int,
    job: jopT,
    indexSets: Belt.Set.Int.t,
  }

  let initData = {index: 0, total: 0, job: Loop, indexSets: Belt.Set.Int.empty}

  let setNextProgramState = (selectedItem, currentItem) => {
    let {index, total} = selectedItem
    switch Belt.Set.Int.has(selectedItem.indexSets, index) {
    | true => {
        ...selectedItem,
        job: Infinite,
      }
    | false =>
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

  let run = (list, selectedItem) => {
    let rec doRun = selectedItem => {
      let updatedItem = setNextProgramState(selectedItem, list->Belt.Array.get(selectedItem.index))
      if updatedItem.job === Loop {
        doRun(updatedItem)
      } else {
        updatedItem
      }
    }

    doRun(selectedItem)
  }

  let swapOperation = code =>
    switch code {
    | Nop(value) => Jmp(value)
    | Jmp(value) => Nop(value)
    | _ => code
    }

  let runSwap = (list, ~updatedIndex) => {
    let program = j => {
      list
      ->Belt.Array.mapWithIndex((i, item) => {
        switch i === j {
        | true => swapOperation(item)
        | false => item
        }
      })
      ->run(initData)
    }

    let rec doRun = i => {
      let updatedProgram = program(i)
      switch updatedProgram.job {
      | Finish | Loop => updatedProgram
      | Infinite => doRun(i - 1)
      }
    }

    doRun(updatedIndex)
  }
}

module Parse = {
  open Program

  let input = item => {
    let re = %re("/(acc|jmp|nop) (\+|\-)(\d+)/")->Js.Re.exec_(item)

    switch re {
    | Some(reResult) => {
        let result =
          reResult
          ->Js.Re.captures
          ->Belt.Array.keepMap(nullableItem => Js.Nullable.toOption(nullableItem))
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
}

let instructionList = inputFromFile->Belt.Array.keepMap(Parse.input)

let p1Runner = Program.run(instructionList)

let part1Program = p1Runner(Program.initData)

let part1 = part1Program.total->Js.log

let part2Program = instructionList->Program.runSwap(~updatedIndex=Belt.Array.length(inputFromFile))

let part2 = part2Program.total->Js.log
