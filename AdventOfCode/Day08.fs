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
let escapeQuote = quote >>= (fun _ -> result Quote)
let backslash = single '\\'
let escapeBackslash = backslash >>= (fun _ -> result Backslash)
let escapeHex =
    single 'x' >>= (fun _ ->
    hexDigit >>= (fun d1 ->
    hexDigit >>= (fun d2 ->
    result (Hex (d1, d2)))))
let escape =
    backslash >>= (fun _ ->
    escapeBackslash <|> escapeQuote <|> escapeHex)
let notQuote = matches ((<>) '\"')
let literalChar = notQuote >>= (Char >> result)
let literalEscaped = escape >>= (Escaped >> result)
let literal = literalEscaped <|> literalChar
let stringLiteral =
    quote >>= (fun _ ->
    many literal >>= (fun x ->
    quote >>= (fun _ ->
    result x)))

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
