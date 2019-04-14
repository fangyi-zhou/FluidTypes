namespace FluidTypes.Refinements

module Typing =
    let empty_ctx : TyCtx = {varCtx = Map.empty; predicateCtx = []}

    let env_add_var (v: Variable) (ty: Ty) (ty_ctx: TyCtx) : TyCtx =
        (* TODO: Renaming *)
        {ty_ctx with varCtx = Map.add v ty ty_ctx.varCtx}

    let env_add_predicate (predicate: Term) (ty_ctx: TyCtx) : TyCtx =
        {ty_ctx with predicateCtx = predicate :: ty_ctx.predicateCtx}

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
        | Var v ->
            match Map.tryFind v var_ctx with
            | Some (BaseType (b, _)) -> Some (BaseType (b, mk_binop_app EqualInt (Var special_this) (Var v)))
            | ty -> ty
        | App (term_1, term_2) ->
            match infer_type ctx term_1 with
            | Some (FuncType (v, t_arg, t_result)) ->
                if check_type ctx term_2 t_arg
                then
                    (* t_result [term_2 / v] *)
                    Some (Substitution.substitute_ty t_result v term_2)
                else None
            | _ ->
                None
        | Anno (term, ty) ->
            if check_type ctx term ty
            then Some ty
            else None
        | Coerce (term, (BaseType (b, _) as ty)) ->
            let base_type_typecheck = check_type ctx term (mk_basetype b) in
            let coercion_type_wf = is_wf_type ctx ty in
            if base_type_typecheck && coercion_type_wf
            then Some ty
            else None
        | _ -> None

    and check_type (ctx: TyCtx) (term: Term) (ty: Ty) : bool =
        if is_wf_type ctx ty
        then begin
            match term with
            | Abs (v, term_) ->
                match ty with
                | FuncType (v', t_arg, t_result) when v = v' ->
                    let ctx = env_add_var v t_arg ctx in
                    check_type ctx term_ t_result
                | FuncType (v', _, _) ->
                    check_type ctx term (Substitution.alpha_conv_ty v' v ty)
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
        let free_vars = FreeVar.free_var_ty ty in
        let var_ctx = ctx.varCtx in
        (* Check FV(ty) is a subset of dom(Gamma) *)
        if Set.forall (fun fv -> Map.containsKey fv var_ctx) free_vars
        then begin
            match ty with
            | BaseType (b, term) ->
                let ctx = env_add_var special_this (mk_basetype b) ctx in
                check_simple_type ctx term (mk_basetype TBool)
            | FuncType (v, t_arg, t_result) ->
                if is_wf_type ctx t_arg
                then begin
                    let ctx_ = env_add_var v t_arg ctx in
                    is_wf_type ctx_ t_result
                end
                else false
            end
        else false

    and eq_simple_ty (ty_1: Ty) (ty_2: Ty) =
        match ty_1, ty_2 with
        | BaseType (base_1, _), BaseType (base_2, _) ->
            base_1 = base_2
        | FuncType (_, t_arg_1, t_result_1), FuncType(_, t_arg_2, t_result_2) ->
            eq_simple_ty t_arg_1 t_arg_2 && eq_simple_ty t_result_1 t_result_2
        | _, _ -> false

    and infer_simple_type (ctx: TyCtx) (term: Term) : Ty option =
        match term with
        | Var _
        | Const _ -> infer_type ctx term
        | App (term_1, term_2) ->
            match infer_simple_type ctx term_1 with
            | Some (FuncType (_, t_arg, t_result)) when check_simple_type ctx term_2 t_arg ->
                Some t_result
            | _ -> None
        | Anno (term_, ty)
        | Coerce (term_, ty) ->
            if check_simple_type ctx term_ ty
            then Some ty
            else None
        | _ -> None

    and check_simple_type (ctx: TyCtx) (term: Term) (ty: Ty) : bool =
        match term with
        | Abs (v, term_) ->
            match ty with
            | FuncType (_, t_arg, t_result) ->
                let ctx' = env_add_var v t_arg ctx in
                check_simple_type ctx' term_ t_result
            | _ -> false
        | IfThenElse (term_cond, term_then, term_else) ->
            check_simple_type ctx term_cond (mk_basetype TBool)
            && check_simple_type ctx term_then ty
            && check_simple_type ctx term_else ty
        | _ ->
            match infer_simple_type ctx term with
            | Some ty_ -> eq_simple_ty ty ty_
            | _ -> false

    and is_subtype (ctx: TyCtx) (ty_1: Ty) (ty_2: Ty): bool =
        match ty_1, ty_2 with
        | BaseType (basety_1, term_1), BaseType (basety_2, term_2) ->
            basety_1 = basety_2
            (* TODO: Load encodingoptions properly *)
            && Encoding.check_subtype ctx term_1 term_2 basety_1
        | FuncType (v_1, t_arg_1, t_result_1), FuncType (v_2, t_arg_2, t_result_2) when v_1 = v_2 ->
            let ctx_ = env_add_var v_1 t_arg_2 ctx in
            is_subtype ctx t_arg_2 t_arg_1
            && is_subtype ctx_ t_result_1 t_result_2
        | FuncType (v_1, t_arg_1, t_result_1), FuncType (v_2, t_arg_2, t_result_2) ->
            is_subtype ctx ty_1 (Substitution.alpha_conv_ty v_2 v_1 t_result_2)
        | _ -> false
