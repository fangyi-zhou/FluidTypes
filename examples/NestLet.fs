module NestLet

let nest x =
  let f a b c = x + a + b + c in
  f 1 2 3 + f 4 5 6
