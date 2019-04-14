module Abs

open FluidTypes.Annotations

[<Refined("(x: {v:int|true}) -> {v: int|v>=0}")>]
let abs x =
    if x > 0
    then x
    else -x
