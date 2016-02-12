﻿module ParserCombinators

open MaybeExpression

let consume =
    function
    | [] -> None
    | x :: xs -> Some (x, xs)
let result x input = maybe { return (x, input) }
let fail = fun _ -> None
let (>>=) parser f input =
    maybe {
        let! (x', input') = parser input
        let! (x'', input'') = f x' input'
        return (x'', input'')
    }

type ParserBuilder() =
    member this.Bind(x, f) = x >>= f
    member this.Return(x) = result x
    member this.ReturnFrom(x) = x
let parser = new ParserBuilder()

let matches p =
    parser {
        let! x = consume
        return! if p x then result x else fail
    }

let single c = matches ((=) c)

let (<|>) parser1 parser2 input =
    let option1 = parser1 input
    match option1 with
    | Some _ -> option1
    | None -> parser2 input

let rec many p =
    parser {
        let! x = p
        let! xs = many p
        return (x :: xs)
    }
    <|>
    result []
let many1 p =
    parser {
        let! x = p
        let! xs = many p
        return (x :: xs)
    }

let parseAll parser input =
    match parser input with
    | Some (x, []) -> Some x
    | _ -> None
