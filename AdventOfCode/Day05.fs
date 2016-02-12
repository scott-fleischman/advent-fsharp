module Day05

let hasAtLeast n p =
    Seq.scan (fun s x -> s + if p x then 1 else 0) 0
    >> Seq.exists (fun x -> x >= n)

let has3Vowels =
    let vowels = set "aeiou"
    let isVowel x = Set.contains x vowels
    hasAtLeast 3 isVowel

let existsPair predicate = Seq.pairwise >> Seq.exists predicate
let hasDoubleLetter = existsPair (fun (x, y) -> x = y)

let hasNoInvalidPair =
    let isInvalidPair (x, y) =
        let invalidPairs = set ["ab"; "cd"; "pq"; "xy"]
        let pairString = System.String [| x; y |]
        Set.contains pairString invalidPairs
    not << existsPair isInvalidPair

let isNice = Utility.andAll [has3Vowels; hasDoubleLetter; hasNoInvalidPair]

let inputLines = System.IO.File.ReadAllLines "Day05Input.txt"
let niceTotal =
    inputLines
    |> Seq.filter isNice
    |> Seq.length
