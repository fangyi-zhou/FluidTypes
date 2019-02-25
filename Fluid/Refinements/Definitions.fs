namespace FluidTypes

[<AutoOpen>]
module Definitions = 

    type Term = 
    | Var of string
    | Const of Constant
    | App of Term * Term
    | Abs of string (* variable *) * Term
    | IfThenElse of Term * Term * Term
    | Anno of Term * Ty
    | Coerce of Term * Ty
    and Ty =
    | BaseType of BaseTy * Term
    | FuncType of string (* variable *) * Ty (* of argument *) * Ty (* of result *)
    and BaseTy =
    | TBool
    | TInt
    and Constant = 
    | IntLiteral of int
    | BoolLiteral of bool
    | Binop of Binop
    | Unop of Unop
    and Binop =
    | Plus
    | Minus
    | And
    | Or
    | EqualInt
    | NotEqualInt
    | EqualBool
    | NotEqualBool
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    and Unop =
    | Not
    | Negate

    type TyCtx = {
        varCtx: Map<string, Ty>;
        predicateCtx: List<Term>;
    }
