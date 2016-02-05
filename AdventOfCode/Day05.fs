module Day05

let vowels = "aeiou" |> Set.ofSeq
let isVowel x = Set.contains x vowels

let has3Vowels = Utility.hasAtLeast 3 isVowel

let invalidPairs = set ["ab"; "cd"; "pq"; "xy"]
let isInvalidPair (x, y) = Set.contains (new string([| x; y |])) invalidPairs 

let matchesPairPredicate predicate = Seq.pairwise >> Seq.exists predicate
let hasDoubleLetter = matchesPairPredicate (function | (x, y) -> x = y)
let hasInvalidPair = not << matchesPairPredicate isInvalidPair

let isNice = Utility.andAll [has3Vowels; hasDoubleLetter; hasInvalidPair]

let inputLines = System.IO.File.ReadAllLines "Day05Input.txt"

let niceTotal = inputLines |> Seq.map isNice |> Utility.getTrueCount
