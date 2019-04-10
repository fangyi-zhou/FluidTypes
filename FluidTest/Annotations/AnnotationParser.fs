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
        let parse = parse_term
        parse "x" |> should equal (Var "x")
        parse "x1" |> should equal (Var "x1")
        parse "x_1" |> should equal (Var "x_1")
