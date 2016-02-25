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

let parsedGates =
    System.IO.File.ReadAllLines "Day07Input.txt"
    |> Seq.map (fun x -> (x, x |> Seq.toList |> parseAll gate))
    |> Seq.where (snd >> Option.isNone)

let answer = parsedGates
