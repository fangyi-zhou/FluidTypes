namespace FluidTest

module TypingTest =
    open NUnit.Framework
    open FluidTypes
    open FluidTypes.Typing
    open FsUnit

    let mk_int i = Const (IntLiteral i)
    let mk_equal_int i = App (App (Const (Binop EqualInt), Var "$this"), mk_int i)
    [<Test>]
    let ``Int Literals have correct types`` () =
        infer_type empty_ctx (mk_int 42) |> should equal (Some (BaseType (TInt, (mk_equal_int 42))))
