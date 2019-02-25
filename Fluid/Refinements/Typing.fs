namespace FluidTypes

module Typing = 

    let empty_ctx : TyCtx = {varCtx = Map.empty; predicateCtx = []}
    
    let mk_this_eq_term (base_ty: BaseTy) (t: Term) : Term =
        let eq =
            match base_ty with
            | TInt -> EqualInt
            | TBool -> EqualBool
        in
        App (App (Const (Binop eq), (Var "$this")), t)
    
    let mk_basetype (base_ty: BaseTy) : Ty =
        BaseType (base_ty, Const (BoolLiteral true))
    
    let mk_binop_type (b: Binop) : Ty =
        let binop_term = App (App (Const (Binop b), (Var "x")), (Var "y")) in
        let ty_arg, ty_result =
            match b with
            | Plus | Minus -> TInt, TInt
            | And | Or | EqualBool | NotEqualBool -> TBool, TBool
            | EqualInt | NotEqualInt | Greater | GreaterEqual | Less | LessEqual -> TInt, TBool
        in
        let refinement_term = mk_this_eq_term ty_result binop_term in
        FuncType ("x", mk_basetype ty_arg, (FuncType ("y", mk_basetype ty_arg, BaseType (ty_result, refinement_term))))
        
    let mk_unop_type (u: Unop) : Ty =
        let unop_term = App (Const (Unop u), (Var "x")) in 
        let ty_arg, ty_result =
            match u with
            | Negate -> TInt, TInt
            | Not -> TBool, TBool
        in
        let refinement_term = mk_this_eq_term ty_result unop_term in
        FuncType ("x", mk_basetype ty_arg, BaseType (ty_result, refinement_term))
    
    let type_const (c: Constant) : Ty =
        match c with
        | IntLiteral _ -> BaseType (TInt, mk_this_eq_term TInt (Const c))
        | BoolLiteral _ -> BaseType (TBool, mk_this_eq_term TBool (Const c))
        | Binop b ->
            mk_binop_type b
        | Unop u ->
            mk_unop_type u

    let rec infer_type (ctx: TyCtx) (term: Term) : Ty option = 
        match term with
        | Const c -> Some (type_const c)
        | _ -> failwith "Unimplemented"
    
    and check_type (ctx: TyCtx) (term: Term) (ty: Ty) : bool =
        false

    and check_simple_type (ctx: TyCtx) (term: Term) (ty: Ty) : bool = 
        false
