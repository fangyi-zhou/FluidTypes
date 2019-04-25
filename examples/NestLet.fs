module NestLet

open FluidTypes.Annotations

let nest x =
  let twice = x + x in
  twice

let nest2 x =
  let twice = x + x in
  let twice_twice = twice + twice in
  twice_twice
