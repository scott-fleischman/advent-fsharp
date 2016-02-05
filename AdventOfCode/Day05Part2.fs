module Day05Part2

type State = IsMatch | Unmatched of Map<string, int>
let hasDoublePair =
    let nextState map pairIndex text =
        match Map.tryFind text map with
        | Some previousIndex -> if previousIndex + 1 <> pairIndex then IsMatch else Unmatched map
        | None -> Unmatched (Map.add text pairIndex map)
    let updateState s p =
        match (s, p) with
        | (IsMatch, _) -> IsMatch
        | (Unmatched map, (pairIndex, text)) -> nextState map pairIndex text
    Seq.pairwise
    >> Seq.mapi (fun index (x, y) -> (index, System.String [| x; y |]))
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
