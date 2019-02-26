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
        
    let env_add_var (v: string) (ty: Ty) (ty_ctx: TyCtx) : TyCtx =
        (* TODO: Renaming *)
        {ty_ctx with varCtx = Map.add v ty ty_ctx.varCtx}
        
    let env_add_predicate (predicate: Term) (ty_ctx: TyCtx) : TyCtx =
        {ty_ctx with predicateCtx = predicate :: ty_ctx.predicateCtx}
    
    let mk_basetype (base_ty: BaseTy) : Ty =
        BaseType (base_ty, Const (BoolLiteral true))
        
    let mk_not (term: Term) : Term =
        App (Const (Unop Not), term)
    
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
        | Binop b -> mk_binop_type b
        | Unop u -> mk_unop_type u

    let rec infer_type (ctx: TyCtx) (term: Term) : Ty option =
        let var_ctx = ctx.varCtx in
        let predicate_ctx = ctx.predicateCtx in
        match term with
        | Const c -> Some (type_const c)
        | Var v -> Map.tryFind v var_ctx
        | App (term_1, term_2) ->
            match infer_type ctx term_1 with
            | Some (FuncType (v, t_arg, t_result)) ->
                if check_type ctx term_2 t_arg
                then (* t_result[t2/v] *) failwith "Unimplemented"
                else None
            | _ ->
                None
        | Anno (term, ty) ->
            if check_type ctx term ty
            then Some ty
            else None
        | _ -> None
    
    and check_type (ctx: TyCtx) (term: Term) (ty: Ty) : bool =
        if is_wf_type ctx ty
        then begin
            match term with
            | Abs (v, term_) ->
                match ty with
                | FuncType (v, t_arg, t_result) ->
                    let ctx = env_add_var v t_arg ctx in
                    check_type ctx term_ t_result
                | _ -> false
            | IfThenElse (term_cond, term_then, term_else) ->
                match infer_type ctx term_cond with
                | Some (BaseType (TBool, _)) ->
                    let ctx_1 = env_add_predicate term_cond ctx in
                    let ctx_2 = env_add_predicate (mk_not term_cond) ctx in
                    check_type ctx_1 term_then ty
                    && check_type ctx_2 term_else ty
                | _ -> false
            | t -> (* Subtyping *)
                match infer_type ctx t with
                | Some ty_inferred ->
                    is_subtype ctx ty_inferred ty (* ty_specified *)
                | None -> false
        end
        else false
        
    and is_wf_type (ctx: TyCtx) (ty: Ty) : bool =
        false

    and check_simple_type (ctx: TyCtx) (term: Term) (ty: Ty) : bool = 
        false
        
    and is_subtype (ctx: TyCtx) (ty_1: Ty) (ty_2: Ty): bool =
        false
