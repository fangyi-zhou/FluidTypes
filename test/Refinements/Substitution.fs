namespace FluidTypes.Test

module SubstitutionTest =
    open FluidTypes.Refinements
    open FluidTypes.Refinements.Substitution
    open FsCheck
    open NUnit.Framework
    open FsUnit
    open FsCheck.NUnit

    let x = Var "x" in
    let y = Var "y" in

    [<Property(EndSize = 10)>]
    let ``x[M/x] = M`` (term: Term) =
        substitute_term x "x" term |> should equal term

    [<Property(EndSize = 10)>]
    let ``y[M/x] = y`` (term: Term) =
        substitute_term y "x" term |> should equal y

    [<Property(EndSize = 10)>]
    let ``Const is invariant under substitution`` (c: Constant) (v: Variable) (term: Term) =
        substitute_term (Const c) v term |> should equal (Const c)

    [<Test>]
    let ``Capture Avoidance when substituting lambdas`` () =
        let ``\x -> y`` = Abs ("x", y) in
        let ``\y -> x`` = Abs ("y", x) in
        let ``\y -> y`` = Abs ("y", y) in
        substitute_term ``\x -> y`` "y" ``\y -> y`` |> should equal (Abs ("x", ``\y -> y``))
        substitute_term ``\x -> y`` "y" ``\y -> x`` |> should equal (Abs ("x_0", ``\y -> x``))

