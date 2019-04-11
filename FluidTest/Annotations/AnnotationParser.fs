namespace FluidTypes.Test

module AnnotationParserTest =
    open NUnit.Framework
    open FsUnit
    open FluidTypes.Refinements
    open FluidTypes.Annotations.AnnotationParser

    [<Test>]
    let ``can parse int/bool literals`` () =
        let parse = parse_term in
        parse "true" |> should equal (mk_bool true)
        parse "false" |> should equal (mk_bool false)
        parse "1" |> should equal (mk_int 1)
        parse "123" |> should equal (mk_int 123)
        parse "-1" |> should equal (mk_int -1)
        parse "-123" |> should equal (mk_int -123)

    [<Test>]
    let ``can parse variables`` () =
        let parse = parse_term in
        parse "x" |> should equal (Var "x")
        parse "x1" |> should equal (Var "x1")
        parse "x_1" |> should equal (Var "x_1")

    [<Test>]
    let ``can parse applications`` () =
        let parse = parse_term in
        let x = Var "x" in
        let y = Var "y" in
        let z = Var "z" in
        let ``x y`` = App (x, y) in
        let ``(x y) z`` = App (``x y``, z) in
        let ``x (y z)`` = App (x, App (y, z)) in
        parse "x y" |> should equal ``x y``
        parse "x y z" |> should equal ``(x y) z``
        parse "(x y) z" |> should equal ``(x y) z``
        parse "x (y z)" |> should equal ``x (y z)``

    [<Test>]
    let ``can parse adds and minuses`` () =
        let parse = parse_term in
        let x = Var "x" in
        let one = mk_int 1 in
        let ``x + 1`` = mk_binop_app Plus x one in
        let ``1 + x`` = mk_binop_app Plus one x in
        let ``1 + 1`` = mk_binop_app Plus one one in
        (* Left assoc *)
        let ``1 + 1 + x`` = mk_binop_app Plus ``1 + 1`` x in
        let ``x - 1`` = mk_binop_app Minus x one in
        let ``1 - x`` = mk_binop_app Minus one x in
        let ``1 - 1`` = mk_binop_app Minus one one in
        (* Left assoc *)
        let ``1 - 1 - x`` = mk_binop_app Minus ``1 - 1`` x in
        let ``1 + 1 - x`` = mk_binop_app Minus ``1 + 1`` x in
        parse "x + 1" |> should equal ``x + 1``
        parse "1 + x" |> should equal ``1 + x``
        parse "1 + 1" |> should equal ``1 + 1``
        parse "1 + 1 + x" |> should equal ``1 + 1 + x``
        parse "x - 1" |> should equal ``x - 1``
        parse "1 - x" |> should equal ``1 - x``
        parse "1 - 1" |> should equal ``1 - 1``
        parse "1 - 1 - x" |> should equal ``1 - 1 - x``
        parse "1 + 1 - x" |> should equal ``1 + 1 - x``

    [<Test>]
    let ``can parse relation operators`` () =
        let parse = parse_term in
        let x = Var "x" in
        let y = Var "y" in
        let ``x > y`` = mk_binop_app Greater x y in
        let ``x <> y`` = mk_binop_app NotEqualInt x y in
        let ``x < y`` = mk_binop_app Less x y in
        let ``x = y`` = mk_binop_app EqualInt x y in
        let ``x >= y`` = mk_binop_app GreaterEqual x y in
        let ``x <= y`` = mk_binop_app LessEqual x y in
        let ``not x`` = mk_not x in
        parse "x > y" |> should equal ``x > y``
        parse "x < y" |> should equal ``x < y``
        parse "x <> y" |> should equal ``x <> y``
        parse "x = y" |> should equal ``x = y``
        parse "x >= y" |> should equal ``x >= y``
        parse "x <= y" |> should equal ``x <= y``
        parse "not x" |> should equal ``not x``

    [<Test>]
    let ``can parse with mixed precedence`` () =
        let parse = parse_term in
        let x = Var "x" in
        let y = Var "y" in
        let one = mk_int 1 in
        let ``x + y`` = mk_binop_app Plus x y in
        let ``x + y = 1`` = mk_binop_app EqualInt ``x + y`` one in
        parse "x + y = 1" |> should equal ``x + y = 1``
        parse "x + y = 1 && x + y = 1" |> should equal (mk_binop_app And ``x + y = 1`` ``x + y = 1``)

    [<Test>]
    let ``can parse logical operators`` () =
        let parse = parse_term in
        let x = Var "x" in
        let y = Var "y" in
        let ``x && y`` = mk_binop_app And x y in
        let ``x || y`` = mk_binop_app Or x y in
        parse "x && y" |> should equal ``x && y``
        parse "x || y" |> should equal ``x || y``
