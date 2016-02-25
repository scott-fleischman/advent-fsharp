module Day07

open ParserCombinators
open ParserCommon

type Wire = Wire of string
type Signal = Signal of uint16
type Connection =
    | Direct of Signal
    | BitwiseComplement of Wire
    | BitwiseAnd of Wire * Wire
    | BitwiseOr of Wire * Wire
    | LeftShift of Wire * Signal
    | RightShift of Wire * Signal
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

let direct = Direct <@> signal
let bitwiseComplement = unary BitwiseComplement "NOT" wire
let bitwiseAnd = binary BitwiseAnd wire "AND" wire
let bitwiseOr = binary BitwiseOr wire "OR" wire
let leftShift = binary LeftShift wire "LSHIFT" signal
let rightShift = binary RightShift wire "RSHIFT" signal
let connection = direct <|> bitwiseComplement <|> bitwiseAnd <|> bitwiseOr <|> leftShift <|> rightShift
let gate = binary Gate connection "->" wire

let answer =
    "x LSHIFT 2 -> c"
    |> Seq.toList
    |> parseAll gate
