module Day08

open MaybeExpression
open ParserCombinators

type Escape =
    | Backslash
    | Quote
    | Hex of char * char

type Literal =
    | Char of char
    | Escaped of Escape

let digit = matches (fun c -> c >= '0' && c <= '9')
let hexLetter = matches (fun c -> c >= 'a' && c <= 'f')
let hexDigit = digit <|> hexLetter
let quote = single '\"'
let escapeQuote =
    parser {
        let! _ = quote
        return Quote
    }
let backslash = single '\\'
let escapeBackslash =
    parser {
        let! _ = backslash
        return Backslash
    }
let escapeHex =
    parser {
        let! _ = single 'x'
        let! d1 = hexDigit
        let! d2 = hexDigit
        return Hex (d1, d2)
    }
let escape =
    parser {
        let! _ = backslash
        return! escapeBackslash <|> escapeQuote <|> escapeHex
    }
let notQuote = matches ((<>) '\"')
let literalChar =
    parser {
        let! c = notQuote
        return Char c
    }
let literalEscaped =
    parser {
        let! x = escape
        return Escaped x
    }
let literal = literalEscaped <|> literalChar
let stringLiteral =
    parser {
        let! _ = quote
        let! x = many literal
        let! _ = quote
        return x
    }

let lines = System.IO.File.ReadAllLines "Day08Input.txt"
let answer =
    let getDifference x =
        maybe {
            let! parseResult = parseAll stringLiteral (Seq.toList x)
            return (Seq.length x - Seq.length parseResult)
        }
    let addOption o1 o2 =
        maybe {
            let! x = o1
            let! y = o2
            return x + y
        }
    lines
    |> Seq.map getDifference
    |> Seq.fold addOption (Some 0)
