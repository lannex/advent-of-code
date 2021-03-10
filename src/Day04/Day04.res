let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n\n")

exception Failed_to_validate

let parseInput = (item: string) => {
  let makeMap =
    item
    ->Js.String2.splitByRe(%re("/\s/"))
    ->Belt.Array.map(optionValue =>
      optionValue->Belt.Option.getWithDefault("")->Js.String2.split(":")
    )
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
      keyValue->Belt.Map.String.get("byr")->Belt.Option.isSome &&
      keyValue->Belt.Map.String.get("iyr")->Belt.Option.isSome &&
      keyValue->Belt.Map.String.get("eyr")->Belt.Option.isSome &&
      keyValue->Belt.Map.String.get("hgt")->Belt.Option.isSome &&
      keyValue->Belt.Map.String.get("hcl")->Belt.Option.isSome &&
      keyValue->Belt.Map.String.get("ecl")->Belt.Option.isSome &&
      keyValue->Belt.Map.String.get("pid")->Belt.Option.isSome

    | None => false
    }
  })
  ->Belt.Array.length
  ->Js.log

module Validate = {
  type rawPassportType = option<string>

  type passportType = {
    byr: rawPassportType,
    iyr: rawPassportType,
    eyr: rawPassportType,
    hgt: rawPassportType,
    hcl: rawPassportType,
    ecl: rawPassportType,
    pid: rawPassportType,
    cid: rawPassportType,
  }

  // type eclType = AMB|BLU|BRN|GRY|GRN|HZL|OTH

  let matchReg = (string, regex) => {
    switch string->Js.String2.match_(regex) {
    | Some(v) => Belt.Array.getExn(v, 0) === string
    | None => false
    }
  }

  let year = (y: rawPassportType, least: int, most: int) => {
    switch y {
    | Some(value) => {
        let v = Garter.Int.fromStringExn(value)
        v >= least && v <= most
      }
    | None => false
    }
  }

  let height = (h: rawPassportType) => {
    switch h {
    | Some(value) => {
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

    | None => false
    }
  }

  let hairColor = (c: rawPassportType) => {
    switch c {
    | Some(value) => matchReg(value, %re("/(#)[0-9a-f]{6}/"))
    | None => false
    }
  }

  let eyeColor = (c: rawPassportType) => {
    switch c {
    | Some(value) =>
      switch value {
      | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
      | _ => false
      }
    | None => false
    }
  }

  let passportId = (id: rawPassportType) => {
    switch id {
    | Some(value) => matchReg(value, %re("/(\d){9}/"))
    | None => false
    }
  }

  let all = mapItem => {
    switch mapItem {
    | Some(item) => {
        let data: passportType = {
          byr: item->Belt.Map.String.get("byr"),
          iyr: item->Belt.Map.String.get("iyr"),
          eyr: item->Belt.Map.String.get("eyr"),
          hgt: item->Belt.Map.String.get("hgt"),
          hcl: item->Belt.Map.String.get("hcl"),
          ecl: item->Belt.Map.String.get("ecl"),
          pid: item->Belt.Map.String.get("pid"),
          cid: item->Belt.Map.String.get("cid"),
        }
        [
          data.byr->year(1920, 2002),
          data.iyr->year(2010, 2020),
          data.eyr->year(2020, 2030),
          data.hgt->height,
          data.hcl->hairColor,
          data.ecl->eyeColor,
          data.pid->passportId,
          // cid: data.cid,
        ]->Belt.Array.every(item => item === true)
      }
    | None => false
    }
  }
}

let part2 =
  inputFromFile
  ->Belt.Array.map(parseInput)
  ->Belt.Array.keep(Validate.all)
  ->Belt.Array.length
  ->Js.log
