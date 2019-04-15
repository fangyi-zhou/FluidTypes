namespace FluidTypes.Tests

open Expecto

module FreeVarTests =
    open FluidTypes.Refinements
    open FluidTypes.Refinements.FreeVar

    let x = Var "x" in
    let y = Var "y" in
    let z = Var "z" in
    let this = Var special_this in

    [<Property(EndSize = 10)>]
    let ``FV of variable is itself`` (var: string) =
        free_var_term (Var var) |> should equal (Set.singleton var)

    [<Property(EndSize = 10)>]
    let ``FV of consts are always empty`` (c: Constant) =
        free_var_term (Const c) |> should equal Set.empty

    [<Property(EndSize = 10)>]
    let ``FV of lambdas does contain the bound variable`` (var: string) (term: Term) =
        free_var_term (Abs (var, term)) |> should not' (contain var)

    [<Property(EndSize = 10)>]
    let ``FV of base types does not contain special this`` (basety: BaseTy) (term: Term) =
        free_var_ty (BaseType (basety, term)) |> should not' (contain special_this)

    [<Test>]
    let ``FV of some terms (w/o types)`` () =
        let expect t vars = free_var_term t |> should equal (Set.ofList vars) in
        expect x ["x"];
        expect (Abs ("x", x)) [];
        expect (Abs ("x", y)) ["y"];
        expect (App (x, y)) ["x"; "y"];
        expect (IfThenElse (x, y, y)) ["x"; "y"]
        expect (IfThenElse (x, x, y)) ["x"; "y"]
        expect (IfThenElse (x, y, z)) ["x"; "y"; "z"]

    [<Test>]
    let ``FV of some types`` () =
        let expect ty vars = free_var_ty ty |> should equal (Set.ofList vars) in
        expect (BaseType (TBool, this)) [];
        expect (BaseType (TBool, x)) ["x"];
        expect (FuncType ("x", mk_basetype TBool, BaseType (TBool, this))) [];
        expect (FuncType ("x", mk_basetype TBool, BaseType (TBool, x))) [];
        expect (FuncType ("x", mk_basetype TBool, BaseType (TBool, y))) ["y"];
        expect (FuncType ("x", BaseType (TBool, this), BaseType (TBool, this))) [];
        expect (FuncType ("x", BaseType (TBool, x), BaseType (TBool, this))) ["x"];
        expect (FuncType ("x", BaseType (TBool, y), BaseType (TBool, this))) ["y"];
        expect (FuncType ("x", BaseType (TBool, this), BaseType (TBool, x))) [];
        expect (FuncType ("x", BaseType (TBool, x), BaseType (TBool, x))) ["x"];
        expect (FuncType ("x", BaseType (TBool, y), BaseType (TBool, x))) ["y"];
        expect (FuncType ("x", BaseType (TBool, this), BaseType (TBool, y))) ["y"];
        expect (FuncType ("x", BaseType (TBool, x), BaseType (TBool, y))) ["x"; "y"];
        expect (FuncType ("x", BaseType (TBool, y), BaseType (TBool, y))) ["y"];
