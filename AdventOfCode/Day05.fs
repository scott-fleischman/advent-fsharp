module Day05

let vowels = "aeiou" |> Set.ofSeq
let isVowel x = Set.contains x vowels
let getTrueCount = Seq.map (fun x -> if x then 1 else 0) >> Seq.sum
let vowelCount (text : string) = text |> Seq.map isVowel |> getTrueCount

let isDoubleLetter = (=)

let invalidTwoLetters = set ["ab"; "cd"; "pq"; "xy"]
let isInvalidTwoLetter x y = Set.contains (new string([| x; y |])) invalidTwoLetters 

let matchesTwoLetterPredicateContext predicate context currentLetter =
    match context with
    | (_, true) -> (None, true)
    | (Some previous, false) ->
        if predicate previous currentLetter
        then (None, true)
        else (Some currentLetter, false)
    | (None, false) -> (Some currentLetter, false)
let matchesTwoLetterPredicate predicate text =
    text
    |> Seq.fold (matchesTwoLetterPredicateContext predicate) (None, false)
    |> snd

let isNice text =
    vowelCount text >= 3 &&
    matchesTwoLetterPredicate isDoubleLetter text &&
    not <| matchesTwoLetterPredicate isInvalidTwoLetter text

let inputLines = System.IO.File.ReadAllLines "Day05Input.txt"

let niceTotal = inputLines |> Seq.map isNice |> getTrueCount
