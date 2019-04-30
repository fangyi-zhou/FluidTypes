module Adder

open FluidTypes.Annotations

[<Refined("(x: {v:int|true}) -> {v:int|v>=x}")>]
let add7 x = x + 7

let add7_anno (x : int) = x + 7
