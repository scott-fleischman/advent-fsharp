module Day08Simple

let count =
    let hexDigits = set <| ['a'..'f'] @ ['A'..'F'] @ ['0'..'9']
    let isHexDigit x = Set.contains x hexDigits

    let rec go n =
        function
        | ['\"'] | [] -> n
        | '\\' :: '\\' :: xs -> go (n + 1) xs
        | '\\' :: '\"' :: xs -> go (n + 1) xs
        | '\\' :: 'x' :: x :: y :: xs when isHexDigit x && isHexDigit y -> go (n + 1) xs
        | x :: xs -> go (n + 1) xs
    Seq.skip 1 >> Seq.toList >> go 0

let answer =
    System.IO.File.ReadAllLines "Day08Input.txt"
    |> Seq.sumBy (fun x -> Seq.length x - count x)
