module Nat

open FluidTypes.Annotations

[<Refined("{v:int|v>=0}")>]
type Nat = int

let abs (x : int) : Nat =
    if x > 0 then x
    else -x
