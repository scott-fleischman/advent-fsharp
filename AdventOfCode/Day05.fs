﻿module Day05

let hasAtLeast n p =
    Seq.scan (fun s x -> s + if p x then 1 else 0) 0
    >> Seq.map (fun x -> x >= n)
    >> Seq.exists id

let has3Vowels =
    let vowels = "aeiou" |> Set.ofSeq
    let isVowel x = Set.contains x vowels
    hasAtLeast 3 isVowel

let existsPair predicate = Seq.pairwise >> Seq.exists predicate
let hasDoubleLetter = existsPair (function | (x, y) -> x = y)

let hasInvalidPair =
    let isInvalidPair (x, y) =
        let invalidPairs = set ["ab"; "cd"; "pq"; "xy"]
        let pairString = System.String [| x; y |]
        Set.contains pairString invalidPairs
    not << existsPair isInvalidPair

let isNice = Utility.andAll [has3Vowels; hasDoubleLetter; hasInvalidPair]

let inputLines = System.IO.File.ReadAllLines "Day05Input.txt"
let niceTotal =
    inputLines
    |> Seq.filter isNice
    |> Seq.length
