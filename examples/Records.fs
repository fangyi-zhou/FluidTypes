module Record

open FluidTypes.Annotations

type IntTuple = {
  [<Refined("{v:int|v>=0}")>] x: int;
  [<Refined("{v:int|v>=x}")>] y: int;
}

let ex1 : IntTuple = {
  x = 10
  y = 12
}

[<Refined("{v:int|v>=0}")>]
let x = ex1.x
[<Refined("{v:int|v>=0}")>]
let y = ex1.y
