module Utility

let constFunction x = (fun _ -> x)

let andAll predicates =
    constFunction
    >> Seq.initInfinite
    >> Seq.zip predicates
    >> Seq.forall (fun (p, x) -> p x)
