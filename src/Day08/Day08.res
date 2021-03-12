let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

exception Failed_to_parse_input

exception Failed_to_loop

module Program = {
  type codeType =
    | Acc(int)
    | Jmp(int)
    | Nop(int)

  type jopType =
    | Finish
    | Loop
    | Infinite

  type resultType = {
    index: int,
    total: int,
    job: jopType,
    indexSets: Belt.Set.Int.t,
  }

  let initData = {index: 0, total: 0, job: Loop, indexSets: Belt.Set.Int.empty}

  let setItem = (selectedItem, index, cb) =>
    switch Belt.Set.Int.has(selectedItem.indexSets, index) {
    | true => {
        ...selectedItem,
        job: Infinite,
      }
    | false => cb()
    }

  let rec run = (list, selectedItem) => {
    let {index, total} = selectedItem
    let currentItem = list->Belt.Array.get(index)

    let updatedItem = switch currentItem {
    | Some(Acc(value)) =>
      setItem(selectedItem, index, () => {
        ...selectedItem,
        index: index + 1,
        total: total + value,
        indexSets: Belt.Set.Int.add(selectedItem.indexSets, index),
      })

    | Some(Jmp(value)) =>
      setItem(selectedItem, index, () => {
        ...selectedItem,
        index: index + value,
        indexSets: Belt.Set.Int.add(selectedItem.indexSets, index),
      })

    | Some(Nop(_)) =>
      setItem(selectedItem, index, () => {
        ...selectedItem,
        index: index + 1,
        indexSets: Belt.Set.Int.add(selectedItem.indexSets, index),
      })

    | None => {
        ...selectedItem,
        job: Finish,
      }
    }

    switch updatedItem.job {
    | Loop => run(list, updatedItem)
    | _ => updatedItem
    }
  }

  let swapOperation = code =>
    switch code {
    | Nop(value) => Jmp(value)
    | Jmp(value) => Nop(value)
    | _ => code
    }

  let rec runSwap = (arr, ~updatedIndex) => {
    let program =
      arr
      ->Belt.Array.mapWithIndex((i, item) => {
        switch i === updatedIndex {
        | true => swapOperation(item)
        | false => item
        }
      })
      ->run(initData)

    switch program.job {
    | Finish | Loop => program
    | Infinite => runSwap(arr, ~updatedIndex=updatedIndex - 1)
    }
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

let program = inputFromFile->Belt.Array.keepMap(Parse.input)->Program.run(Program.initData)

let part1 = program.total->Js.log

let part2 =
  inputFromFile
  ->Belt.Array.keepMap(Parse.input)
  ->Program.runSwap(~updatedIndex=Belt.Array.length(inputFromFile))
  ->Js.log
