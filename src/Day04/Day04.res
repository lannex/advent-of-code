let inputFromFile = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split("\n\n")

type passportT = {
  byr: string,
  iyr: string,
  eyr: string,
  hgt: string,
  hcl: string,
  ecl: string,
  pid: string,
  cid: option<string>,
}

// type field = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID
type hgt = CM(int) | IN(int)

type ecl =
  | AMB
  | BLU
  | BRN
  | GRY
  | GRN
  | HZL
  | OTH

type validPassportT = {
  byr: int,
  iyr: int,
  eyr: int,
  hgt: hgt,
  hcl: string,
  ecl: ecl,
  pid: string,
  cid: option<string>,
}

module Passport = {
  let matchReg = (string, regex) => {
    switch string->Js.String2.match_(regex) {
    | Some(v) if Belt.Array.getExn(v, 0) === string => string
    | Some(_) | None => raise(Not_found)
    }
  }

  let year = (value: string, least: int, most: int) => {
    let strToInt = Garter.Int.fromStringExn(value)
    strToInt >= least && strToInt <= most ? strToInt : raise(Not_found)
  }

  let height = (value: string) => {
    let result = value->Js.String2.splitByRe(%re("/(\d+)(cm|in)/"))
    switch result->Belt.Array.get(1) {
    | Some(_) => {
        let [_, v, unit, _] = result

        // Checked on above
        let n = v->Belt.Option.getExn->Garter.Int.fromStringExn

        switch unit->Belt.Option.getWithDefault("") {
        | "cm" if n >= 150 && n <= 193 => CM(n)
        | "in" if n >= 59 && n <= 76 => IN(n)
        | _ => raise(Not_found)
        }
      }
    | None => raise(Not_found)
    }
  }

  let hairColor = value => value->matchReg(%re("/(#)[0-9a-f]{6}/"))

  let eyeColor = (value: string) => {
    switch value {
    | "amb" => AMB
    | "blu" => BLU
    | "brn" => BRN
    | "gry" => GRY
    | "grn" => GRN
    | "hzl" => HZL
    | "oth" => OTH
    | _ => raise(Not_found)
    }
  }

  let passportId = value => value->matchReg(%re("/(\d){9}/"))
}

module Parser = {
  let captureKeyValue = item =>
    item->Belt.Option.flatMap(str => {
      str
      ->Utils.Re.captures(%re("/(byr|iyr|eyr|hgt|hcl|ecl|pid|cid)(:)(.*)/"))
      ->Belt.Option.flatMap(line => {
        switch line {
        | [_, key, _, value] => Some((key, value))
        | _ => None
        }
      })
    })

  let makeInputToMap = rawLine => {
    let result = rawLine->Js.String2.splitByRe(%re("/\s/"))->Belt.Array.keepMap(captureKeyValue)
    if Belt.Array.length(result) > 0 {
      Some(result->Belt.Map.String.fromArray)
    } else {
      None
    }
  }

  let passport = mapItem => {
    try {
      let result: passportT = {
        byr: mapItem->Belt.Map.String.getExn("byr"),
        // ->Belt.Option.flatMap(Belt.Int.fromString),
        iyr: mapItem->Belt.Map.String.getExn("iyr"),
        // ->Belt.Option.flatMap(Belt.Int.fromString),
        eyr: mapItem->Belt.Map.String.getExn("eyr"),
        // ->Belt.Option.flatMap(Belt.Int.fromString),
        hgt: mapItem->Belt.Map.String.getExn("hgt"),
        // ->Belt.Option.getWithDefault(""),
        hcl: mapItem->Belt.Map.String.getExn("hcl"),
        // ->Belt.Option.getWithDefault(""),
        ecl: mapItem->Belt.Map.String.getExn("ecl"),
        // ->Belt.Option.getWithDefault(""),
        pid: mapItem->Belt.Map.String.getExn("pid"),
        // ->Belt.Option.getWithDefault(""),
        cid: mapItem->Belt.Map.String.get("cid"),
      }
      Some(result)
    } catch {
    | Not_found => None
    }
  }

  let validPassport = (mapItem: passportT) => {
    open Passport
    try {
      let result: validPassportT = {
        byr: mapItem.byr->year(1920, 2002),
        iyr: mapItem.iyr->year(2010, 2020),
        eyr: mapItem.eyr->year(2020, 2030),
        hgt: mapItem.hgt->height,
        hcl: mapItem.hcl->hairColor,
        ecl: mapItem.ecl->eyeColor,
        pid: mapItem.pid->passportId,
        cid: mapItem.cid,
      }
      Some(result)
    } catch {
    | Not_found => None
    }
  }
}

let count = arr => arr->Belt.Array.length

let parsedInput = inputFromFile->Belt.Array.keepMap(Parser.makeInputToMap)

let part1 = parsedInput->Belt.Array.keepMap(Parser.passport)->count->Js.log
// 256

let part2 =
  parsedInput
  ->Belt.Array.keepMap(Parser.passport)
  ->Belt.Array.keepMap(Parser.validPassport)
  ->count
  ->Js.log
// 198
