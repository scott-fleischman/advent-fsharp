module Day05

let vowels = "aeiou" |> Set.ofSeq
let isVowel x = Set.contains x vowels
let getTrueCount = Seq.sumBy (function | true -> 1 | false -> 0)
let vowelCount (text : string) = text |> Seq.map isVowel |> getTrueCount

let hasAtLeast n p =
    Seq.scan (fun s x -> s + if p x then 1 else 0) 0
    >> Seq.tryPick (fun x -> if x >= n then Some () else None)
    >> Option.isSome
let has3Vowels = hasAtLeast 3 isVowel

let isDoubleLetter (x, y) = x = y

let invalidPairs = set ["ab"; "cd"; "pq"; "xy"]
let isInvalidPair (x, y) = Set.contains (new string([| x; y |])) invalidPairs 

let matchesPairPredicate predicate = Seq.pairwise >> Seq.exists predicate

let isNice text =
    has3Vowels text &&
    matchesPairPredicate isDoubleLetter text &&
    not <| matchesPairPredicate isInvalidPair text

let inputLines = System.IO.File.ReadAllLines "Day05Input.txt"

let niceTotal = inputLines |> Seq.map isNice |> getTrueCount
