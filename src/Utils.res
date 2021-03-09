module Array = {
  let getFirst = arr => Belt.Array.get(arr, 0)

  let getLast = arr => Belt.Array.get(arr, Belt.Array.length(arr) - 1)
}
