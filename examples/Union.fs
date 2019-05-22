module Union

open FluidTypes.Annotations

type Number =
    | [<Refined("{v:int|v>=0}")>] NonNeg of int
    | [<Refined("{v:int|v<0}")>] Neg of int

[<Refined("(x: Number) -> {v:int|v>=0}")>]
let numberAbs (x: Number) =
    match x with
    | NonNeg nonneg -> nonneg
    | Neg neg -> neg
