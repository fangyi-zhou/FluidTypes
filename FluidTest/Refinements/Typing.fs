namespace FluidTypes.Test

module TypingTest =
    open NUnit.Framework
    open FluidTypes
    open FluidTypes.Typing
    open FsUnit

    [<Test>]
    let ``Int Literals have correct types`` () =
        infer_type empty_ctx (mk_int 42) |> should equal (Some (BaseType (TInt, (mk_equal_int 42))))


