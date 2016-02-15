module Day08Simple

let lines = System.IO.File.ReadAllLines "Day08Input.txt"

let hexDigits = set <| ['a'..'f'] @ ['A'..'F'] @ ['0'..'9']
let isHexDigit x = Set.contains x hexDigits

let count s =
    let s' = s |> Seq.skip 1 |> Seq.toList
    let rec go n =
        function
        | ['\"'] | [] -> n
        | '\\' :: '\\' :: xs -> go (n + 1) xs
        | '\\' :: '\"' :: xs -> go (n + 1) xs
        | '\\' :: 'x' :: x :: y :: xs when isHexDigit x && isHexDigit y -> go (n + 1) xs
        | x :: xs -> go (n + 1) xs
    go 0 s'

let answer =
    lines
    |> Seq.sumBy (fun x -> Seq.length x - count x)
