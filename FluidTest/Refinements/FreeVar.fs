namespace FluidTypes.Test

module FreeVarTest =
    open NUnit.Framework
    open FluidTypes
    open FluidTypes.FreeVar
    open FsCheck.NUnit
    open FsUnit

    [<Property>]
    let ``FV of variable is itself`` (var: string) =
        free_var_term (Var var) |> should equal (Set.singleton var)

    [<Property>]
    let ``FV of consts are always empty`` (c: Constant) =
        free_var_term (Const c) |> should equal Set.empty

    [<Property>]
    let ``FV of lambdas does contain the bound variable`` (var: string) (term: Term) =
        free_var_term (Abs (var, term)) |> should not' (contain var)

    [<Property>]
    let ``FV of base types does not contain special this`` (basety: BaseTy) (term: Term) =
        free_var_ty (BaseType (basety, term)) |> should not' (contain special_this)
