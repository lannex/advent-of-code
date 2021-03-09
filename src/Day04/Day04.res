let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n\n")

exception Fail_to_read_input

let testInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"->Js.String2.split("\n\n")

type passportType = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

let parseInput = (item: string) => {
  let makeMap =
    item
    ->Js.String2.splitByRe(%re("/\s/"))
    ->Belt.Array.map(value => Js.String2.split(value->Belt.Option.getWithDefault(""), ":"))
    ->Belt.Array.map(keyValue => (
      Utils.Array.getFirst(keyValue)->Belt.Option.getWithDefault(""),
      Utils.Array.getLast(keyValue)->Belt.Option.getWithDefault(""),
    ))
    ->Belt.Map.String.fromArray
  Some(makeMap)
}

let part1 =
  inputFromFile
  ->Belt.Array.map(parseInput)
  ->Belt.Array.keep(mapItem => {
    switch mapItem {
    | Some(keyValue) =>
      switch Belt.Option.isSome(Belt.Map.String.get(keyValue, "byr")) &&
      Belt.Option.isSome(Belt.Map.String.get(keyValue, "iyr")) &&
      Belt.Option.isSome(Belt.Map.String.get(keyValue, "eyr")) &&
      Belt.Option.isSome(Belt.Map.String.get(keyValue, "hgt")) &&
      Belt.Option.isSome(Belt.Map.String.get(keyValue, "hcl")) &&
      Belt.Option.isSome(Belt.Map.String.get(keyValue, "ecl")) &&
      Belt.Option.isSome(Belt.Map.String.get(keyValue, "pid")) {
      | true => true
      | false => false
      }
    | None => false
    }
  })
  ->Belt.Array.length
  ->Js.log
