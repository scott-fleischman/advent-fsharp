module ParserCommon

open ParserCombinators

let parseDigit =
    function
    | '0' -> Some 0
    | '1' -> Some 1
    | '2' -> Some 2
    | '3' -> Some 3
    | '4' -> Some 4
    | '5' -> Some 5
    | '6' -> Some 6
    | '7' -> Some 7
    | '8' -> Some 8
    | '9' -> Some 9
    | _ -> None
let digitValue = parseDigit <@> consume >>= failOnNone
let digitsToNumber ds = List.fold (fun n d -> n * 10 + d) 0 ds
let number = digitsToNumber <@> many1 digitValue
let space = matches System.Char.IsWhiteSpace
let spaces = many1 space

let letter = matches System.Char.IsLetter
let letters =
    let toString cs = cs |> Seq.toArray |> System.String
    toString <@> many1 letter
