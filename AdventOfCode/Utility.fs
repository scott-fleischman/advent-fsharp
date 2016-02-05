module Utility

let constFunction x = (fun _ -> x)

let andAll predicates =
    constFunction
    >> Seq.initInfinite
    >> Seq.zip predicates
    >> Seq.exists (function | (p, x) -> not (p x))
    >> not
