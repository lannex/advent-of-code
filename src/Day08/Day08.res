let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n")

exception Failed_to_parse_input

let testInput = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
qweqwe
-qweqw"->Js.String2.split("\n")

module Program = {
  type operationType =
    | Nop
    | Acc
    | Jmp

  type codeType = Code(operationType, int)

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

  let setItem = (selectedItem, index, cb) =>
    switch Belt.Set.Int.has(selectedItem.indexSets, index) {
    | true => {
        ...selectedItem,
        job: Infinite,
      }
    | false => cb()
    }

  let rec runProgram = (list, selectedItem) => {
    let {index, total} = selectedItem
    let currentItem = list->Belt.Array.get(index)

    let updatedItem = switch currentItem {
    | Some(Code(Nop, _)) =>
      setItem(selectedItem, index, () => {
        ...selectedItem,
        index: index + 1,
        indexSets: Belt.Set.Int.add(selectedItem.indexSets, index),
      })

    | Some(Code(Acc, value)) =>
      setItem(selectedItem, index, () => {
        ...selectedItem,
        index: index + 1,
        total: total + value,
        indexSets: Belt.Set.Int.add(selectedItem.indexSets, index),
      })

    | Some(Code(Jmp, value)) =>
      setItem(selectedItem, index, () => {
        ...selectedItem,
        index: index + value,
        indexSets: Belt.Set.Int.add(selectedItem.indexSets, index),
      })

    | None => {
        ...selectedItem,
        job: Finish,
      }
    }

    switch updatedItem.job {
    | Loop => runProgram(list, updatedItem)
    | Finish | _ => updatedItem
    }
  }
}

module Parse = {
  open Program

  let input = item => {
    let re = %re("/(nop|acc|jmp) (\+|\-)(\d+)/")->Js.Re.exec_(item)

    switch re {
    | Some(reResult) => {
        let result =
          reResult
          ->Js.Re.captures
          ->Belt.Array.keepMap(nullableItem => Js.Nullable.toOption(nullableItem))
        let operation = Belt.Array.getExn(result, 1)->(
          op => {
            switch op {
            | "acc" => Acc
            | "jmp" => Jmp
            | "nop" => Nop
            | _ => raise(Not_found) //
            }
          }
        )
        let argument =
          (Belt.Array.getExn(result, 2) ++ Utils.Array.getLastExn(result))->Garter.Int.fromStringExn
        Some(Code(operation, argument))
      }
    | None => None
    }
  }
}

let program =
  inputFromFile
  ->Belt.Array.map(Parse.input)
  ->Belt.Array.keepMap(item => item)
  ->Program.runProgram({index: 0, total: 0, job: Loop, indexSets: Belt.Set.Int.empty})

let part1 = program.total->Js.log
