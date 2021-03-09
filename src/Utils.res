module Array = {
  let getFirst = arr => Belt.Array.getExn(arr, 0)

  let getLast = arr => Belt.Array.getExn(arr, Belt.Array.length(arr) - 1)
}
