let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n\n")

module Passport = {
  type part1T = {
    byr: string,
    iyr: string,
    eyr: string,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option<string>,
  }

  type part2T = {
    byr: int,
    iyr: int,
    eyr: int,
    hgt: string,
    hcl: string,
    ecl: string,
    pid: string,
    cid: option<string>,
  }

  let validateKeys = kv => {
    let isSome = (item, key) => item->Belt.Map.String.get(key)->Belt.Option.isSome
    isSome(kv, "byr") &&
    isSome(kv, "iyr") &&
    isSome(kv, "eyr") &&
    isSome(kv, "hgt") &&
    isSome(kv, "hcl") &&
    isSome(kv, "ecl") &&
    isSome(kv, "pid")
  }

  let count = arr => arr->Belt.Array.length

  let matchReg = (string, regex) => {
    switch string->Js.String2.match_(regex) {
    | Some(v) => Belt.Array.getExn(v, 0) === string
    | None => false
    }
  }

  let year = (value, least: int, most: int) => {
    value >= least && value <= most
  }

  let height = value => {
    let arr = Js.String2.splitByRe(value, %re("/(\d+)(cm|in)/"))
    switch Belt.Array.get(arr, 1) {
    | Some(_) => {
        let [_, v, unit, _] = Js.String2.splitByRe(value, %re("/(\d+)(cm|in)/"))
        let n = v->Belt.Option.getExn->Garter.Int.fromStringExn

        switch unit->Belt.Option.getWithDefault("") {
        | "cm" => n >= 150 && n <= 193
        | "in" => n >= 59 && n <= 76
        | _ => false
        }
      }
    | None => false
    }
  }

  let hairColor = value => {
    matchReg(value, %re("/(#)[0-9a-f]{6}/"))
  }

  let eyeColor = value => {
    switch value {
    | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
    | _ => false
    }
  }

  let passportId = value => {
    matchReg(value, %re("/(\d){9}/"))
  }

  let validateAll = obj => {
    switch obj {
    | Some(item) => {
        let isValid = [
          item.byr->year(1920, 2002),
          item.iyr->year(2010, 2020),
          item.eyr->year(2020, 2030),
          item.hgt->height,
          item.hcl->hairColor,
          item.ecl->eyeColor,
          item.pid->passportId,
          // cid: data.cid,
        ]->Belt.Array.every(v => v === true)
        Some(isValid)
      }
    | None => None
    }
  }
}

module Parse = {
  let input = (rawLine: string) => {
    let result =
      rawLine
      ->Js.String2.splitByRe(%re("/\s/"))
      ->Belt.Array.keepMap(item => {
        switch item {
        | Some(v) =>
          let capturedLine = v->Utils.Re.captures(%re("/(byr|iyr|eyr|hgt|hcl|ecl|pid|cid)(:)(.*)/"))
          switch capturedLine {
          | Some(line) => {
              let [_, key, _, value] = line
              Some((key, value))
            }
          | None => None
          }
        | None => None
        }
      })
    switch Belt.Array.length(result) > 0 {
    | true => {
        let item = result->Belt.Map.String.fromArray
        Some(item)
      }
    | false => None
    }
  }

  let passport = (mapItem): option<Passport.part2T> => {
    switch mapItem {
    | Some(item) =>
      Some({
        byr: item
        ->Belt.Map.String.get("byr")
        ->Belt.Option.getWithDefault("0")
        ->Garter.Int.fromStringExn,
        iyr: item
        ->Belt.Map.String.get("iyr")
        ->Belt.Option.getWithDefault("0")
        ->Garter.Int.fromStringExn,
        eyr: item
        ->Belt.Map.String.get("eyr")
        ->Belt.Option.getWithDefault("0")
        ->Garter.Int.fromStringExn,
        hgt: item->Belt.Map.String.get("hgt")->Belt.Option.getWithDefault(""),
        hcl: item->Belt.Map.String.get("hcl")->Belt.Option.getWithDefault(""),
        ecl: item->Belt.Map.String.get("ecl")->Belt.Option.getWithDefault(""),
        pid: item->Belt.Map.String.get("pid")->Belt.Option.getWithDefault(""),
        cid: item->Belt.Map.String.get("cid"),
      })
    | None => None
    }
  }
}

let part1 =
  inputFromFile
  ->Belt.Array.map(Parse.input)
  ->Belt.Array.keepMap(mapItem => {
    switch mapItem {
    | Some(kv) => {
        let hasKeys = Passport.validateKeys(kv)
        hasKeys ? Some(hasKeys) : None
      }
    | None => None
    }
  })
  ->Passport.count
  ->Js.log
// 256

let part2 =
  inputFromFile
  ->Belt.Array.map(Parse.input)
  ->Belt.Array.map(Parse.passport)
  ->Belt.Array.keepMap(Passport.validateAll)
  ->Belt.Array.keep(v => v)
  ->Passport.count
  ->Js.log
// 198
