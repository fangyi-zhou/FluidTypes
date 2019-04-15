namespace FluidTypes.Tests

open Expecto

module TypingTests =
    open FluidTypes.Refinements
    open FluidTypes.Refinements.Typing

    [<Property(EndSize = 10)>]
    let ``Int Literals have correct types`` (x: int) =
        infer_type empty_ctx (mk_int x) |> should equal (Some (BaseType (TInt, (mk_equal_int x))))

    [<Property(EndSize = 10)>]
    let ``Bool Literals have correct types`` (x: bool) =
        infer_type empty_ctx (mk_bool x) |> should equal (Some (BaseType (TBool, (mk_equal_bool x))))

    [<Test>]
    let ``Subtyping for ints are reflexive`` () =
        let ty_1 = BaseType (TInt, (mk_equal_int 1)) in
        is_subtype empty_ctx ty_1 ty_1 |> should equal true

    [<Test>]
    let ``$this = n is a subtype of $this >= n`` () =
        let n = 1 in
        let ty_1 = BaseType (TInt, (mk_equal_int n)) in
        let ty_2 = BaseType (TInt, (mk_binop_app GreaterEqual (Var special_this) (mk_int n))) in
        is_subtype empty_ctx ty_1 ty_2 |> should equal true

    [<Test>]
    let ``$this = n is a subtype of $this >= n where n is in a context`` () =
        let n = 1 in
        let ctx = empty_ctx in
        let ctx = env_add_var "n" (BaseType (TInt, mk_equal_int n)) ctx in
        let n_term = Var "n" in
        let ty_1 = BaseType (TInt, (mk_binop_app EqualInt (Var special_this) n_term)) in
        let ty_2 = BaseType (TInt, (mk_binop_app GreaterEqual (Var special_this) n_term)) in
        is_subtype ctx ty_1 ty_2 |> should equal true

    [<Test>]
    let ``Well-formed Coercions are typable`` () =
        let ctx = empty_ctx in
        let ctx = env_add_var "x" (mk_basetype TInt) ctx in
        let x = Var "x" in
        let ty = BaseType (TInt, mk_equal_int 1) in
        infer_type ctx (Coerce (x, ty)) |> should equal (Some ty)

    [<Test>]
    let ``Ill-formed Coercions are not typable`` () =
        let ctx = empty_ctx in
        let ctx = env_add_var "x" (mk_basetype TInt) ctx in
        let x = Var "x" in
        let y = Var "y" in
        let ty = BaseType (TInt, mk_binop_app EqualInt x y) in
        infer_type ctx (Coerce (x, ty)) |> should equal None

    [<Test>]
    let ``Addition of int consts are typeable`` () =
        (* For some reason, this test hangs with FsCheck *)
        let x = 42 in
        let y = -2 in
        let x_ = mk_int x in
        let y_ = mk_int y in
        let b = Plus in
        let ``x + y`` = mk_binop_app b x_ y_ in
        infer_type empty_ctx ``x + y`` |> should equal (Some (BaseType (TInt, mk_this_eq_term TInt ``x + y``)))
