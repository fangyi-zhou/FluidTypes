namespace FluidTypes.Refinements

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
        | Let of Variable * Term * Term
        | UnknownTerm of string * Ty
        | NewRecord of Term list * string
        | FieldGet of Term * string
        | Tuple of Term list
        | Diverge

    and Ty =
        | BaseType of BaseTy * Term
        | FuncType of Variable * Ty * (* of argument *) Ty
        (* of result *)
        | UnknownType of string
        | RecordType of string
        | UnionType of string
        | ProductType of Ty list

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

    type RecordDef = (string * Ty) list
    type UnionDef = (string * Ty) list

    type RecordDefMap = Map<string, RecordDef>
    type UnionDefMap = Map<string, UnionDef>

    type TyCtx =
        { varCtx : Map<Variable, Ty>
          predicateCtx : List<Term>
          recordDef : RecordDefMap
          unionDef: UnionDefMap }

    let special_this : Variable = "$this"

    type EncodingEnv =
        { consts : Map<Variable, BaseTy>
          functions : Map<Variable, Variable * Term>
          clauses : Set<Term>
          ensure_encoded_clauses : Set<Term> }
