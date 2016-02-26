module Day07

open ParserCombinators
open ParserCommon

type Wire = Wire of string
type Signal = Signal of uint16
type Input =
    | SignalInput of Signal
    | WireInput of Wire
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

let reduce m _ c =
    let reduceInput input =
        match input with
        | SignalInput _ -> input
        | WireInput w ->
            match Map.tryFind w m with
            | Some (Direct (SignalInput s)) -> SignalInput s
            | Some _ | None -> input

    let reduceUnary cons f i =
        match reduceInput i with
        | SignalInput s -> f s |> SignalInput |> Direct
        | WireInput _ as i' -> cons i'

    let reduceBinary cons f i1 i2 =
        match (reduceInput i1, reduceInput i2) with
        | (SignalInput s1, SignalInput s2) -> f s1 s2 |> SignalInput |> Direct
        | (i1', i2') -> cons (i1', i2')

    match c with
    | Direct i -> Direct (reduceInput i)
    | BitwiseComplement x -> reduceUnary BitwiseComplement SignalOps.bitwiseComplement x
    | BitwiseAnd (x1, x2) -> reduceBinary BitwiseAnd SignalOps.bitwiseAnd x1 x2
    | BitwiseOr (x1, x2) -> reduceBinary BitwiseOr SignalOps.bitwiseOr x1 x2
    | LeftShift (x1, s2) -> reduceUnary (fun x -> LeftShift (x, s2)) (fun x -> SignalOps.leftShift x s2) x1
    | RightShift (x1, s2) -> reduceUnary (fun x -> RightShift (x, s2)) (fun x -> SignalOps.rightShift x s2) x1

let makeWireMap =
    List.map (fun (Gate (c, w)) -> (w, c))
    >> Map.ofSeq

let isReducible =
    function
    | Direct (SignalInput _) -> true
    | _ -> false

let rec reduceAll m =
    if Map.exists (fun _ v -> isReducible v) m
    then
        let m' = Map.map (reduce m) m
        if m' = m
        then m
        else reduceAll m'
    else m

let getGateValues = makeWireMap >> reduceAll

let parsedGates =
    System.IO.File.ReadAllLines "Day07Input.txt"
    |> Seq.map (Seq.toList >> parseAll gate)
    |> sequence
    |> Option.map getGateValues
    |> Option.bind (Map.tryFind (Wire "a"))

let answer = parsedGates
