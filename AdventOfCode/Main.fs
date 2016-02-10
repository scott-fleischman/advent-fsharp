// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Main

[<EntryPoint>]
let main argv = 
    printfn "Day 5: %A" (Day05.niceTotal)
    printfn "Day 5 Part 2: %A" (Day05Part2.niceTotal)
    printfn "Day 8: %A" (Day08.result)
    0 // return an integer exit code
