namespace FluidTypes.Refinements

open Microsoft.FSharp.Compiler.SourceCodeServices

[<AutoOpen>]
module Definitions =
    type Variable = string

    type Term =
        | Var of Variable
        | Const of Constant
        | App of Term * Term
        | Abs of Variable * Term
        | IfThenElse of Term * Term * Term
        | Anno of Term * Ty
        | Coerce of Term * Ty
        | UnknownTerm of string * Ty
        | FieldGet of Term * string

    and Ty =
        | BaseType of BaseTy * Term
        | FuncType of Variable * Ty * (* of argument *) Ty
        (* of result *)
        | UnknownType of string
        | RecordType of string

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

    type RecordDef = Map<string, Ty>

    type RecordDefMap = Map<string, RecordDef>

    type TyCtx =
        { varCtx : Map<Variable, Ty>
          predicateCtx : List<Term>
          recordDef : RecordDefMap }

    let special_this : Variable = "$this"

    type EncodingEnv =
        { consts : Map<Variable, BaseTy>
          functions : Map<Variable, Variable * Term>
          clauses : Set<Term> }
