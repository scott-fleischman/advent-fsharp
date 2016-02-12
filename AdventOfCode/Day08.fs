module Day08

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
        let parseResult = parseAll stringLiteral (Seq.toList x)
        match parseResult with
        | Some r -> Some (Seq.length x - Seq.length r)
        | None -> None
    let addOption o1 o2 =
        match (o1, o2) with
        | (Some x, Some y) -> Some (x + y)
        | _ -> None
    lines
    |> Seq.map getDifference
    |> Seq.fold addOption (Some 0)
