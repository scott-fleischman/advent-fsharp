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
    let cachedLookup m w f =
        match Map.tryFind w m with
        | Some x -> (m, x)
        | None ->
            let result = f w
            (Map.add w result m, result)
    let rec runInput m =
        function
        | SignalInput s -> (m, s)
        | WireInput w ->
            cachedLookup m w (fun x -> runConnection m wireMap.[x] |> snd)
    and cachedUnary m f x =
        match runInput m x with
        | (m', r) -> (m', f r)
    and cachedBinary m x f y =
        match runInput m x with
        | (m', x') ->
            match runInput m' y with
            | (m'', y') -> (m'', f x' y')
    and runConnection m c =
        match c with
        | Direct x -> runInput m x
        | BitwiseComplement x -> cachedUnary m SignalOps.bitwiseComplement x
        | BitwiseAnd (x1, x2) -> cachedBinary m x1 SignalOps.bitwiseAnd x2
        | BitwiseOr (x1, x2) -> cachedBinary m x1 SignalOps.bitwiseOr x2
        | LeftShift (x1, s2) -> cachedUnary m (fun x -> SignalOps.leftShift x s2) x1
        | RightShift (x1, s2) -> cachedUnary m (fun x -> SignalOps.rightShift x s2) x1
    runInput Map.empty (WireInput w) |> snd

let parsedGates =
    System.IO.File.ReadAllLines "Day07Input.txt"
    |> Seq.map (Seq.toList >> parseAll gate)
    |> sequence
    |> Option.map (getGateValue (Wire "a"))

let answer = parsedGates
