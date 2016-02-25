module Day07

open ParserCombinators
open ParserCommon

type Wire = Wire of string
type Signal = Signal of uint16
type Input =
    | WireInput of Wire
    | SignalInput of Signal
type Connection =
    | Direct of Input
    | BitwiseComplement of Input
    | BitwiseAnd of Input * Input
    | BitwiseOr of Input * Input
    | LeftShift of Input * Signal
    | RightShift of Input * Signal
type Gate = Gate of Connection * Wire

let unary cons op x =
    cons
    <@> (string op *> spaces *> x)
let curry f x y = f (x, y)
let binary cons left op right =
    curry cons
    <@> (left <* spaces <* string op <* spaces)
    <*> right

let wire = Wire <@> letters
let signal = (fun n -> Signal (uint16 n)) <@> number
let input =
    (WireInput <@> wire)
    <|> (SignalInput <@> signal)

let direct = Direct <@> input
let bitwiseComplement = unary BitwiseComplement "NOT" input
let bitwiseAnd = binary BitwiseAnd input "AND" input
let bitwiseOr = binary BitwiseOr input "OR" input
let leftShift = binary LeftShift input "LSHIFT" signal
let rightShift = binary RightShift input "RSHIFT" signal
let connection = bitwiseComplement <|> bitwiseAnd <|> bitwiseOr <|> leftShift <|> rightShift <|> direct
let gate = binary Gate connection "->" wire

let sequence xs =
    let (>>=) x f = Option.bind f x
    let rec aux s x =
        s >>= (fun ys ->
        x >>= (fun y ->
        Some (y :: ys)))
    Seq.fold aux (Some []) xs

module SignalOps =
    let unary f (Signal x) = Signal (f x)
    let binary f (Signal x) (Signal y) = Signal (f x y)
    let bitwiseComplement = unary (~~~)
    let bitwiseOr = binary (|||)
    let bitwiseAnd = binary (&&&)
    let leftShift = binary (fun x y -> x <<< int32 y)
    let rightShift = binary (fun x y -> x >>> int32 y)

let getGateValue w gs =
    let wireMap =
        gs
        |> Seq.map (fun (Gate (c, w)) -> (w, c))
        |> Map.ofSeq
    let rec runInput =
        function
        | SignalInput s -> s
        | WireInput w -> runConnection wireMap.[w]
    and runConnection =
        function
        | Direct x -> runInput x
        | BitwiseComplement x -> SignalOps.bitwiseComplement (runInput x)
        | BitwiseAnd (x1, x2) -> SignalOps.bitwiseAnd (runInput x1) (runInput x2)
        | BitwiseOr (x1, x2) -> SignalOps.bitwiseOr (runInput x1) (runInput x2)
        | LeftShift (x1, s2) -> SignalOps.leftShift (runInput x1) s2
        | RightShift (x1, s2) -> SignalOps.rightShift (runInput x1) s2
    runInput (WireInput w)

let parsedGates =
    System.IO.File.ReadAllLines "Day07Input.txt"
    |> Seq.map (Seq.toList >> parseAll gate)
    |> sequence
    |> Option.map (getGateValue (Wire "g"))

let answer = parsedGates
