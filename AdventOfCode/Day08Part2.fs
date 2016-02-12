module Day08Part2

let expand =
    function
    | '\\' -> ['\\'; '\\']
    | '\"' -> ['\\'; '\"']
    | x -> [x]
let lines = System.IO.File.ReadAllLines "Day08Input.txt"
let answer =
    let newSize x =
        let encodedLength = x |> Seq.collect expand |> Seq.length
        2 + encodedLength
    lines
    |> Seq.map (fun x -> newSize x - Seq.length x)
    |> Seq.sum
