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
        return! f x' input'
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

let failOnNone x =
    parser {
        return!
            match x with
            | Some y -> result y
            | None -> fail
    }

let single c = matches ((=) c)

let (<|>) parser1 parser2 input =
    let option1 = parser1 input
    match option1 with
    | Some _ -> option1
    | None -> parser2 input

let ( *> ) p1 p2 =
    parser {
        let! _ = p1
        let! r2 = p2
        return r2
    }

let ( <* ) p1 p2 =
    parser {
        let! r1 = p1
        let! _ = p2
        return r1
    }

let ( <@> ) f p =
    parser {
        let! r = p
        return (f r)
    }

let (<*>) pf p2 =
    parser {
        let! f = pf
        let! r = p2
        return (f r)
    }

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

let string s =
    let rec aux =
        function
        | [] -> result []
        | (x :: xs) ->
            parser {
                let! c = consume
                if c = x
                then
                    let! xs = aux xs
                    return! result (x :: xs)
                else
                    return! fail
            }
    aux (s |> Seq.toList)

let parseAll parser input =
    match parser input with
    | Some (x, []) -> Some x
    | _ -> None
