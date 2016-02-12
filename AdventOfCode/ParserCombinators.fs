module ParserCombinators

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
let matches p = consume >>= (fun x -> if p x then result x else fail)
let single c = matches ((=) c)
let (<|>) parser1 parser2 input =
    let option1 = parser1 input
    match option1 with
    | Some _ -> option1
    | None -> parser2 input
let rec many p =
    (p >>= (fun x ->
    many p >>= (fun xs ->
    result (x :: xs))))
    <|>
    result []
let many1 p =
    p >>= (fun x ->
    many p >>= (fun xs ->
    result (x :: xs)))

let parseAll parser input =
    match parser input with
    | Some (x, []) -> Some x
    | _ -> None
