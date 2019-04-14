namespace FluidTypes.Test

module SolverTest =
    open FluidTypes.Refinements.Solver
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
        run_solver sample_formula_1 |> should contain "unsat";
        is_unsat sample_formula_1 |> should equal true


    [<Test>]
    let ``Sample Formula 2 is SAT`` () =
        let result = run_solver sample_formula_2 in
        result |> should contain "sat";
        result |> should not' (contain "unsat");
        is_unsat sample_formula_2 |> should equal false

