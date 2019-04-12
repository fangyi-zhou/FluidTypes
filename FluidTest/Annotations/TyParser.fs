namespace FluidTypes.Test

module TyParserTest =
    open NUnit.Framework
    open FsUnit
    open FluidTypes.Refinements
    open FluidTypes.Annotations.AnnotationParser

    [<Test>]
    let ``can parse base types`` () =
        let parse = parse_ty in
        let ``this = 1`` = BaseType (TInt, mk_equal_int 1) in
        parse "{v:int|true}" |> should equal (mk_basetype TInt)
        parse "{v:bool|true}" |> should equal (mk_basetype TBool)
        parse "{v:int|v = 1}" |> should equal ``this = 1``
        parse "{x:int|x = 1}" |> should equal ``this = 1``

    [<Test>]
    let ``can parse function types in unabbreviated form`` () =
        let parse = parse_ty in
        let ``(x: {v:int|true}) -> {v:int|v=x}`` =
            FuncType ("x", mk_basetype TInt, BaseType (TInt, mk_binop_app EqualInt (Var special_this) (Var "x")))
        in
        parse "(x: {v:int|true}) -> {v:int|v=x}" |> should equal ``(x: {v:int|true}) -> {v:int|v=x}``
        parse "(x: {v:int|true}) -> (y: {v:int|true}) -> {v:int|v=x+y}" |> should equal (mk_binop_type Plus)
