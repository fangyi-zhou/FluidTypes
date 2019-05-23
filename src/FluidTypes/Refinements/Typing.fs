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
    let env_lookup_record (name : Variable) (ty_ctx : TyCtx) : RecordDef option =
        Map.tryFind name ty_ctx.recordDef
    let env_resolve_unknown_ty (name : string) (typedef : Ty) (ty_ctx : TyCtx) : TyCtx =
        { varCtx = Map.map (fun _ -> Substitution.resolve_unknown_ty_in_ty name typedef) ty_ctx.varCtx
          predicateCtx = List.map (Substitution.resolve_unknown_ty_in_term name typedef) ty_ctx.predicateCtx
          recordDef = Map.map (fun _ -> List.map (fun (f, ty) -> f, Substitution.resolve_unknown_ty_in_ty name typedef ty)) ty_ctx.recordDef
        }

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
        | FieldGet(term_, field) ->
            match infer_type ctx term_ with
            | Some(RecordType(r)) ->
                match env_lookup_record r ctx with
                | Some def ->
                    match List.tryFind (fun f -> fst f = field) def with
                    | Some(_, BaseType(base_ty, _)) -> Some(BaseType(base_ty, mk_binop_app EqualInt (Var special_this) term))
                    | Some(_, ty) -> Some ty
                    | None ->
                        err_field_not_found r field
                        None
                | None -> failwithf "Internal error: Missing record definition for %s" r
            | Some _ ->
                err_not_a_record (term_.ToString())
                None
            | None ->
                None
        | NewRecord(terms, record) ->
            if is_wf_record ctx terms record then
                Some (RecordType record)
            else
                None
        | Let(var, t1, t2) ->
            match infer_type ctx t1 with
            | Some ty_1 ->
                let ctx = env_add_var var ty_1 ctx
                match infer_type ctx t2 with
                | Some ty_2 -> Some (Substitution.substitute_ty ty_2 var t1)
                | None -> None
            | None -> None
        | _ ->
            err_not_inferrable (term.ToString())
            None

    and is_wf_record ctx terms record =
        match env_lookup_record record ctx with
        | Some def ->
            let def_pairs = List.zip def terms
            let check_field ctx ((name, ty), term) =
                let checks = check_type ctx term ty
                let ctx = env_add_var name ty ctx
                let ctx = env_add_predicate (mk_binop_app EqualInt (Var name) term) ctx
                checks, env_add_var name ty ctx
            let checks, _ = List.mapFold check_field ctx def_pairs
            List.forall (fun x -> x) checks
        | None -> failwithf "Internal error: Missing record definition for %s" record

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
             | Tuple(terms) ->
                 match ty with
                 | ProductType tys when (List.length terms = List.length tys) ->
                    List.forall2 (check_type ctx) terms tys
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
             | RecordType _ -> true
             | ProductType tys -> List.forall (is_wf_type ctx) tys)
        else
            (let undef_vars = Set.toList undef_vars
             err_not_closed_type (ty.ToString()) undef_vars
             false)

    and eq_simple_ty (ty_1 : Ty) (ty_2 : Ty) =
        match ty_1, ty_2 with
        | BaseType(base_1, _), BaseType(base_2, _) -> base_1 = base_2
        | FuncType(_, t_arg_1, t_result_1), FuncType(_, t_arg_2, t_result_2) ->
            eq_simple_ty t_arg_1 t_arg_2 && eq_simple_ty t_result_1 t_result_2
        | UnknownType t1, UnknownType t2 ->
            remove_namespace t1 = remove_namespace t2 (* FIXME: Handle Namespace correctly *)
        | RecordType r_1, RecordType r_2 -> r_1 = r_2
        | ProductType tys1, ProductType tys2 when (List.length tys1 = List.length tys2) ->
            List.forall2 eq_simple_ty tys1 tys2
        | _, _ -> false

    and infer_simple_type (ctx : TyCtx) (term : Term) : Ty option =
        match term with
        | Var _
        | Const _
        | FieldGet _ -> infer_type ctx term
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
            is_subtype ctx ty_1 (Substitution.alpha_conv_ty v_2 v_1 ty_2)
        | UnknownType ty_1, UnknownType ty_2 ->
            remove_namespace ty_1 = remove_namespace ty_2 (* FIXME: Handle Namespace correctly *)
        | RecordType r_1, RecordType r_2 -> r_1 = r_2
        | ProductType tys1, ProductType tys2 when (List.length tys1 = List.length tys2) ->
            List.forall2 (is_subtype ctx) tys1 tys2
        | _ -> false
