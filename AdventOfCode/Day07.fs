module Day07

open ParserCombinators

let build a b c = (a, b, c)

let digit = matches (fun x -> x >= '0' && x <= '9')
let letter = matches (fun x -> x >= 'a' && x <= 'z')
let test1 = build <@> digit <*> letter <*> letter

let answer =
    "4ab"
    |> Seq.toList
    |> parseAll test1
