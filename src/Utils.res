module Array = {
  let getFirst = arr => Belt.Array.get(arr, 0)

  let getFirstExn = arr => Belt.Array.get(arr, 0)->Belt.Option.getExn

  let getLast = arr => Belt.Array.get(arr, Belt.Array.length(arr) - 1)

  let getLastExn = arr => getLast(arr)->Belt.Option.getExn
}
