namespace FluidTest

module TypingTest =
    open NUnit.Framework
    open FluidTypes
    open FluidTypes.Typing
    open FsUnit

    [<Test>]
    let ``Int Literals have correct types`` () =
        infer_type empty_ctx (mk_int 42) |> should equal (Some (BaseType (TInt, (mk_equal_int 42))))


module CheckSolver =
    open FluidTypes.Solver
    open NUnit.Framework
    open FsUnit
    
    let sample_formula_1 (* UNSAT *) = """(declare-const b Int)
(declare-const x Int)
(assert (= b (+ x 0)))
(assert (not (= b x)))
(check-sat)
"""

    let sample_formula_2 (* SAT *) = """(declare-const b Int)
(declare-const x Int)
(assert (= b (+ x 0)))
(assert (= b x))
(check-sat)
"""
    [<Test>]
    let ``Sample Formula 1 is UNSAT`` () =
        run_solver default_options sample_formula_1 |> should contain "unsat"
        
    [<Test>]
    let ``Sample Formula 2 is SAT`` () =
        let result = run_solver default_options sample_formula_2 in
        result |> should contain "sat"
        result |> should not' (contain "unsat")
