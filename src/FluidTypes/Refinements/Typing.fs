namespace FluidTypes.Refinements

module Typing =
    open FluidTypes.Errors

    let empty_ctx : TyCtx =
        { varCtx = Map.empty
          predicateCtx = []
          recordDef = Map.empty }

    let env_add_var (v : Variable) (ty : Ty) (ty_ctx : TyCtx) : TyCtx =
        { (* TODO: Renaming *)
          ty_ctx with varCtx = Map.add v ty ty_ctx.varCtx }
    let env_add_predicate (predicate : Term) (ty_ctx : TyCtx) : TyCtx =
        { ty_ctx with predicateCtx = predicate :: ty_ctx.predicateCtx }
    let env_add_record (name : Variable) (def : RecordDef) (ty_ctx : TyCtx) : TyCtx =
        { ty_ctx with recordDef = Map.add name def ty_ctx.recordDef }

    let type_const (c : Constant) : Ty =
        match c with
        | IntLiteral _ -> BaseType(TInt, mk_this_eq_term TInt (Const c))
        | BoolLiteral _ -> BaseType(TBool, mk_this_eq_term TBool (Const c))
        | Binop b -> mk_binop_type b
        | Unop u -> mk_unop_type u

    let rec infer_type (ctx : TyCtx) (term : Term) : Ty option =
        let var_ctx = ctx.varCtx
        let predicate_ctx = ctx.predicateCtx
        match term with
        | Const c -> Some(type_const c)
        | Var v ->
            match Map.tryFind v var_ctx with
            | Some(BaseType(b, _)) ->
                Some
                    (BaseType
                         (b, mk_binop_app EqualInt (Var special_this) (Var v)))
            | Some ty -> Some ty
            | None ->
                err_var_not_found v
                None
        | App(term_1, term_2) ->
            match infer_type ctx term_1 with
            | Some(FuncType(v, t_arg, t_result)) ->
                if check_type ctx term_2 t_arg then
                    (* t_result [term_2 / v] *)
                    Some(Substitution.substitute_ty t_result v term_2)
                else None
            | Some ty ->
                err_not_a_function (term_1.ToString()) (ty.ToString())
                None
            | _ -> None
        | Anno(term, ty) ->
            if check_type ctx term ty then Some ty
            else None
        | Coerce(term, (BaseType(b, _) as ty)) ->
            let base_type_typecheck = check_type ctx term (mk_basetype b)
            let coercion_type_wf = is_wf_type ctx ty
            if base_type_typecheck && coercion_type_wf then Some ty
            else None
        | UnknownTerm(_, ty) -> Some ty
        | _ ->
            err_not_inferrable (term.ToString())
            None

    and check_type (ctx : TyCtx) (term : Term) (ty : Ty) : bool =
        if is_wf_type ctx ty then
            (match term with
             | Abs(v, term_) ->
                 match ty with
                 | FuncType(v', t_arg, t_result) when v = v' ->
                     let ctx = env_add_var v t_arg ctx
                     check_type ctx term_ t_result
                 | FuncType(v', _, _) ->
                     check_type ctx term (Substitution.alpha_conv_ty v' v ty)
                 | _ -> false
             | IfThenElse(term_cond, term_then, term_else) ->
                 match infer_type ctx term_cond with
                 | Some(BaseType(TBool, _)) ->
                     let ctx_1 = env_add_predicate term_cond ctx
                     let ctx_2 = env_add_predicate (mk_not term_cond) ctx
                     check_type ctx_1 term_then ty
                     && check_type ctx_2 term_else ty
                 | _ -> false
             | t ->
                 (* Subtyping *)
                 match infer_type ctx t with
                 | Some ty_inferred ->
                     if is_subtype ctx ty_inferred ty (* ty_specified *)
                                                      then true
                     else
                         err_not_a_subtype (term.ToString())
                             (ty_inferred.ToString()) (ty.ToString())
                         false
                 | None -> false)
        else false

    and is_wf_type (ctx : TyCtx) (ty : Ty) : bool =
        let free_vars = FreeVar.free_var_ty ty
        let var_ctx = ctx.varCtx
        (* Check FV(ty) is a subset of dom(Gamma) *)
        let undef_vars =
            Set.filter (fun fv -> not (Map.containsKey fv var_ctx)) free_vars
        if Set.isEmpty undef_vars then
            (match ty with
             | BaseType(b, term) ->
                 let ctx = env_add_var special_this (mk_basetype b) ctx
                 check_simple_type ctx term (mk_basetype TBool)
             | FuncType(v, t_arg, t_result) ->
                 if is_wf_type ctx t_arg then
                     (let ctx_ = env_add_var v t_arg ctx
                      is_wf_type ctx_ t_result)
                 else false
             | UnknownType _ -> true
             | RecordType _ -> true)
        else
            (let undef_vars = Set.toList undef_vars
             err_not_closed_type (ty.ToString()) undef_vars
             false)

    and eq_simple_ty (ty_1 : Ty) (ty_2 : Ty) =
        match ty_1, ty_2 with
        | BaseType(base_1, _), BaseType(base_2, _) -> base_1 = base_2
        | FuncType(_, t_arg_1, t_result_1), FuncType(_, t_arg_2, t_result_2) ->
            eq_simple_ty t_arg_1 t_arg_2 && eq_simple_ty t_result_1 t_result_2
        | _, _ -> false

    and infer_simple_type (ctx : TyCtx) (term : Term) : Ty option =
        match term with
        | Var _
        | Const _ -> infer_type ctx term
        | App(term_1, term_2) ->
            match infer_simple_type ctx term_1 with
            | Some(FuncType(_, t_arg, t_result)) ->
                if check_simple_type ctx term_2 t_arg then Some t_result
                else None
            | Some ty ->
                err_not_a_function (term_1.ToString()) (ty.ToString())
                None
            | _ -> None
        | Anno(term_, ty)
        | Coerce(term_, ty) ->
            if check_simple_type ctx term_ ty then Some ty
            else None
        | UnknownTerm(_, ty) -> Some ty
        | _ ->
            err_not_inferrable (term.ToString())
            None

    and check_simple_type (ctx : TyCtx) (term : Term) (ty : Ty) : bool =
        match term with
        | Abs(v, term_) ->
            match ty with
            | FuncType(_, t_arg, t_result) ->
                let ctx' = env_add_var v t_arg ctx
                check_simple_type ctx' term_ t_result
            | _ ->
                err_not_a_function (term.ToString()) (ty.ToString())
                false
        | IfThenElse(term_cond, term_then, term_else) ->
            let checks_cond =
                check_simple_type ctx term_cond (mk_basetype TBool)
            let checks_then = check_simple_type ctx term_then ty
            let checks_else = check_simple_type ctx term_else ty
            checks_cond && checks_then && checks_else
        | _ ->
            match infer_simple_type ctx term with
            | Some ty_ -> eq_simple_ty ty ty_
            | _ -> false

    and is_subtype (ctx : TyCtx) (ty_1 : Ty) (ty_2 : Ty) : bool =
        match ty_1, ty_2 with
        | BaseType(basety_1, term_1), BaseType(basety_2, term_2) ->
            basety_1 = basety_2
            && Encoding.check_subtype ctx term_1 term_2 basety_1
        | FuncType(v_1, t_arg_1, t_result_1), FuncType(v_2, t_arg_2, t_result_2) when v_1 = v_2 ->
            let ctx_ = env_add_var v_1 t_arg_2 ctx
            is_subtype ctx t_arg_2 t_arg_1
            && is_subtype ctx_ t_result_1 t_result_2
        | FuncType(v_1, t_arg_1, t_result_1), FuncType(v_2, t_arg_2, t_result_2) ->
            is_subtype ctx ty_1 (Substitution.alpha_conv_ty v_2 v_1 t_result_2)
        | UnknownType ty_1, UnknownType ty_2 -> ty_1 = ty_2
        | _ -> false
