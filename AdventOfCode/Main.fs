// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Main

[<EntryPoint>]
let main argv = 
    printfn "Day 5: %A" (Day05.niceTotal)
    printfn "Day 5 pt2: %A" (Day05Part2.niceTotal)
    printfn "Day 7: %A" (Day07.answer)
    printfn "Day 8: %A" (Day08.answer)
    printfn "Day 8': %A" (Day08Simple.answer)
    printfn "Day 8 pt2: %A" (Day08Part2.answer)
    0 // return an integer exit code
