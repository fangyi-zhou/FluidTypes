namespace FluidTypes.Tests

open Expecto

module SolverTests =
    open FluidTypes.Refinements.Solver

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

    [<Tests>]
    let test_unsat =
        testList "run_solver on UNSAT formula"
            [ test "solver result contains unsat"
                  {
                  Expect.contains (run_solver sample_formula_1) "unsat"
                      "solver result should contain unsat" }

              test "is_unsat returns true"
                  {
                  Expect.isTrue (is_unsat sample_formula_1)
                      "is_unsat should return true" } ]

    [<Tests>]
    let tests_sat =
        testList "run_solver on SAT formula"
            [ test "solver result contains sat"
                  {
                  Expect.contains (run_solver sample_formula_2) "sat"
                      "solver result should contain sat" }

              test "is_unsat returns false"
                  {
                  Expect.isFalse (is_unsat sample_formula_2)
                      "is_unsat should return false" } ]
