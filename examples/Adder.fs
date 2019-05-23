module Adder

open FluidTypes.Annotations

[<Refined("(x: {v:int|true}) -> {v:int|v>=x}")>]
let add7 x = x + 7

let add7_anno (x : int) = x + 7

let add2_7 x y = x + y + 7
let add_7_7 x = x + 7 + 7

let add3 x =
    let two = 2
    x + two

let add4 x =
    let two () = 2
    x + two ()
