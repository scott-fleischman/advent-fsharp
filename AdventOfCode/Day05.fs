module Day05

let vowels = "aeiou" |> Set.ofSeq
let isVowel x = Set.contains x vowels
let getTrueCount = Seq.map (fun x -> if x then 1 else 0) >> Seq.sum
let vowelCount (text : string) = text |> Seq.map isVowel |> getTrueCount

let isDoubleLetter (x, y) = x = y

let invalidPairs = set ["ab"; "cd"; "pq"; "xy"]
let isInvalidPair (x, y) = Set.contains (new string([| x; y |])) invalidPairs 

let matchesPairPredicate predicate = Seq.pairwise >> Seq.exists predicate

let isNice text =
    vowelCount text >= 3 &&
    matchesPairPredicate isDoubleLetter text &&
    not <| matchesPairPredicate isInvalidPair text

let inputLines = System.IO.File.ReadAllLines "Day05Input.txt"

let niceTotal = inputLines |> Seq.map isNice |> getTrueCount
