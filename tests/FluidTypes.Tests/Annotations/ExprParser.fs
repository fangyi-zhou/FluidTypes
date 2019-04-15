namespace FluidTypes.Tests

open Expecto

module ExprParserTests =
    open FluidTypes.Refinements
    open FluidTypes.Annotations.AnnotationParser

    let parse = parse_term in
    let mk_test (input, term) =
        test input {
            Expect.equal (parse input) term (input + " is not parsed")
        }

    [<Tests>]
    let ``can parse int/bool literals`` =
        let cases = [
            "true", (mk_bool true)
            "false", (mk_bool false)
            "1", (mk_int 1)
            "123", (mk_int 123)
            "-1", (mk_int -1)
            "-123", (mk_int -123)]
        in
        testList "can parse int/bool literals" (List.map mk_test cases)

    [<Tests>]
    let ``can parse variables`` () =
        let cases = [
            "x", (Var "x")
            "x1",  (Var "x1")
            "x_1", (Var "x_1")]
        in
        testList "can parse variables" (List.map mk_test cases)

    [<Tests>]
    let ``can parse applications`` () =
        let x = Var "x" in
        let y = Var "y" in
        let z = Var "z" in
        let ``x y`` = App (x, y) in
        let ``(x y) z`` = App (``x y``, z) in
        let ``x (y z)`` = App (x, App (y, z)) in
        let cases = [
            "x y", ``x y``
            "x y z", ``(x y) z``
            "(x y) z", ``(x y) z``
            "x (y z)", ``x (y z)``]
        in
        testList "can parse applications" (List.map mk_test cases)

    [<Tests>]
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
        let cases = [
            "x + 1", ``x + 1``
            "1 + x", ``1 + x``
            "1 + 1", ``1 + 1``
            "1 + 1 + x", ``1 + 1 + x``
            "x - 1", ``x - 1``
            "1 - x", ``1 - x``
            "1 - 1", ``1 - 1``
            "1 - 1 - x", ``1 - 1 - x``
            "1 + 1 - x", ``1 + 1 - x``]
        in
        testList "can parse adds and minuses" (List.map mk_test cases)

    [<Tests>]
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
        let cases = [
            "x > y", ``x > y``
            "x < y", ``x < y``
            "x <> y", ``x <> y``
            "x = y", ``x = y``
            "x >= y", ``x >= y``
            "x <= y", ``x <= y``
            "not x", ``not x``]
        in
        testList "can parse relation operators" (List.map mk_test cases)

    [<Tests>]
    let ``can parse with mixed precedence`` () =
        let parse = parse_term in
        let x = Var "x" in
        let y = Var "y" in
        let one = mk_int 1 in
        let ``x + y`` = mk_binop_app Plus x y in
        let ``x + y = 1`` = mk_binop_app EqualInt ``x + y`` one in
        let cases = [
            "x + y = 1", ``x + y = 1``
            "x + y = 1 && x + y = 1", (mk_binop_app And ``x + y = 1`` ``x + y = 1``)]
        in
        testList "can parse with mixed precedence" (List.map mk_test cases)

    [<Tests>]
    let ``can parse logical operators`` () =
        let parse = parse_term in
        let x = Var "x" in
        let y = Var "y" in
        let ``x && y`` = mk_binop_app And x y in
        let ``x || y`` = mk_binop_app Or x y in
        let cases = [
            "x && y", ``x && y``
            "x || y", ``x || y``]
        in
        testList "can parse logical operators" (List.map mk_test cases)
