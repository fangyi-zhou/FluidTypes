namespace FluidTypes.Tests

open Expecto

module TyParserTests =
    open FluidTypes.Refinements
    open FluidTypes.Annotations.AnnotationParser

    let parse = parse_ty
    let mk_test (input, ty) =
        test input { Expect.equal (parse input) ty (input + " is not parsed") }

    [<Tests>]
    let ``can parse base types``() =
        let ``this = 1`` = BaseType(TInt, mk_equal_int 1)

        let cases =
            [ "{v:int|true}", (mk_basetype TInt)
              "{v:bool|true}", (mk_basetype TBool)
              "{v:int|v = 1}", ``this = 1``
              "{x:int|x = 1}", ``this = 1`` ]
        testList "can parse base types" (List.map mk_test cases)

    [<Tests>]
    let ``can parse function types in unabbreviated form``() =
        let parse = parse_ty
        let ``(x: {v:int|true}) -> {v:int|v=x}`` =
            FuncType
                ("x", mk_basetype TInt,
                 BaseType
                     (TInt, mk_binop_app EqualInt (Var special_this) (Var "x")))

        let cases =
            [ "(x: {v:int|true}) -> {v:int|v=x}",
              ``(x: {v:int|true}) -> {v:int|v=x}``

              "(x: {v:int|true}) -> (y: {v:int|true}) -> {v:int|v=x+y}",
              (mk_binop_type Plus) ]
        testList "can parse function types in unabbreviated form"
            (List.map mk_test cases)
