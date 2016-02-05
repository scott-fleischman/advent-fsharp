module Utility

let hasAtLeast n p =
    Seq.scan (fun s x -> s + if p x then 1 else 0) 0
    >> Seq.map (fun x -> x >= n)
    >> Seq.exists id

let andAll functions item =
    Seq.initInfinite (fun _ -> item)
    |> Seq.zip functions
    |> Seq.map (function | (f, x) -> f x)
    |> Seq.exists not
    |> not

let getTrueCount x = x |> Seq.sumBy (function | true -> 1 | false -> 0)
