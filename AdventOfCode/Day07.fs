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

let wire = Wire <@> letters
let signal = (fun n -> Signal (uint16 n)) <@> number
let direct = Direct <@> signal
let bitwiseComplement = BitwiseComplement <@> (string "NOT" *> spaces *> wire)
let connection = direct <|> bitwiseComplement
let gate =
    (fun x y -> Gate (x, y))
    <@> (connection <* spaces)
    <*> (string "->" *> spaces *> wire)

let answer =
    "1900 -> x"
    |> Seq.toList
    |> parseAll gate
