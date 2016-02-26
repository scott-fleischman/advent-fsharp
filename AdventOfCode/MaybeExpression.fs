module MaybeExpression

type MaybeBuilder() =
    member this.Bind(x, f) = Option.bind f x
    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x

let maybe = new MaybeBuilder()
