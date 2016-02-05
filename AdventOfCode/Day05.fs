module Day05

let vowels = "aeiou" |> Set.ofSeq
let isVowel x = Set.contains x vowels

let hasAtLeast n p =
    Seq.scan (fun s x -> s + if p x then 1 else 0) 0
    >> Seq.map (fun x -> x >= n)
    >> Seq.exists id
let has3Vowels = hasAtLeast 3 isVowel

let invalidPairs = set ["ab"; "cd"; "pq"; "xy"]
let isInvalidPair (x, y) = Set.contains (new string([| x; y |])) invalidPairs 

let matchesPairPredicate predicate = Seq.pairwise >> Seq.exists predicate
let hasDoubleLetter = matchesPairPredicate (function | (x, y) -> x = y)
let hasInvalidPair = not << matchesPairPredicate isInvalidPair

let andAll functions item =
    Seq.initInfinite (fun _ -> item)
    |> Seq.zip functions
    |> Seq.map (function | (f, x) -> f x)
    |> Seq.exists not
    |> not

let isNice = andAll [has3Vowels; hasDoubleLetter; hasInvalidPair]

let inputLines = System.IO.File.ReadAllLines "Day05Input.txt"

let getTrueCount = Seq.sumBy (function | true -> 1 | false -> 0)
let niceTotal = inputLines |> Seq.map isNice |> getTrueCount
