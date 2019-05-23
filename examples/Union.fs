module Union

open FluidTypes.Annotations

type Number =
    | [<Refined("{v:int|v>0}")>] Pos of int
    | [<Refined("{v:int|v=0}")>] Zero of int
    | [<Refined("{v:int|v<0}")>] Neg of int

[<Refined("(x: Number) -> {v:int|v>=0}")>]
let numberAbs (x: Number) =
    match x with
    | Pos pos -> pos
    | Zero zero -> zero
    | Neg neg -> -neg

let flip (x: Number) =
    match x with
    | Pos pos -> Neg -pos
    | Zero zero -> Zero zero
    | Neg neg -> Pos -neg
