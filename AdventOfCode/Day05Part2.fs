module Day05Part2

type Pair = Pair of char * char
type PairIndex = PairIndex of int
type State = IsMatch | Unmatched of Map<Pair, PairIndex>
let hasDoublePair =
    let areAdjacent (PairIndex x) (PairIndex y) = x + 1 = y
    let nextState map (pairIndex, pair) =
        match Map.tryFind pair map with
        | Some previousIndex ->
            if areAdjacent previousIndex pairIndex
            then Unmatched map
            else IsMatch
        | None -> Unmatched (Map.add pair pairIndex map)
    let updateState s p =
        match s with
        | IsMatch -> IsMatch
        | Unmatched map -> nextState map p
    Seq.pairwise
    >> Seq.mapi (fun index pair -> (PairIndex index, Pair pair))
    >> Seq.scan updateState (Unmatched Map.empty)
    >> Seq.exists (function | IsMatch -> true | Unmatched _ -> false)

let hasRepeatBetween =
    Seq.windowed 3
    >> Seq.exists (fun x -> x.[0] = x.[2])

let isNice = Utility.andAll [hasDoublePair; hasRepeatBetween]

let inputLines = System.IO.File.ReadAllLines "Day05Input.txt"
let niceTotal =
    inputLines
    |> Seq.filter isNice
    |> Seq.length
