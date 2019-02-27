namespace FluidTypes

module Substitution =


    let find_replacement_var (v: Variable) (excludes: Set<Variable>) : Variable =
        let rec aux i =
            let var = sprintf "%s_%d" v i in
            if Set.contains var excludes
            then aux (i+1)
            else var
        in
        aux 0

    let rec substitute_term (term: Term) (x: Variable) (replace: Term) : Term =
        let sub term = substitute_term term x replace in
        match term with
        | Var v when x = v -> replace
        | Var v -> term
        | Const c -> term
        | Abs (v, _) when x = v -> term
        | Abs (v, term_) ->
            let free_vars = FreeVar.free_var_term replace in
            if Set.contains v free_vars
            then sub (alpha_conv_term v (find_replacement_var v free_vars) term)
            else Abs (v, sub term_)
        | App (term_1, term_2) ->
            App (sub term_1, sub term_2)
        | IfThenElse (term_cond, term_then, term_else) ->
            IfThenElse (sub term_cond, sub term_then, sub term_else)
        | Anno (term_, ty) ->
            Anno (sub term_, substitute_ty ty x replace)
        | Coerce (term_, ty) ->
            Coerce (sub term_, substitute_ty ty x replace)

    and substitute_ty (ty: Ty) (x: Variable) (replace: Term) : Ty =
        match ty with
        | BaseType (basety, term) ->
            BaseType (basety, substitute_term term x replace)
        | FuncType (v, _, _) when x = v ->
            ty
        | FuncType (v, t_arg, t_result) ->
            let sub ty = substitute_ty ty x replace in
            let free_vars = FreeVar.free_var_term replace in
            if Set.contains v free_vars
            then sub (alpha_conv_ty v (find_replacement_var v free_vars) ty)
            else FuncType (v, sub t_arg, sub t_result)

    and alpha_conv_term (v_from: Variable) (v_to: Variable) (term: Term) : Term =
        (* Replace all bound `v_from` to `v_to` *)
        let conv term = alpha_conv_term v_from v_to term in
        match term with
        | Var v when v = v_from -> Var v_to
        | Abs (v, term_) when v = v_from -> Abs (v_to, substitute_term term_ v (Var v_to))
        | Abs (v, term_) -> Abs (v, conv term_)
        | App (term_1, term_2) ->
            App (conv term_1, conv term_2)
        | IfThenElse (term_cond, term_then, term_else) ->
            IfThenElse (conv term_cond, conv term_then, conv term_else)
        | Anno (term_, ty) ->
            Anno (conv term, alpha_conv_ty v_from v_to ty)
        | Coerce (term_, ty) ->
            Coerce (conv term, alpha_conv_ty v_from v_to ty)
        | _ -> term

    and alpha_conv_ty (v_from: Variable) (v_to: Variable) (ty: Ty) : Ty =
        (* Replace all bound `v_from` to `v_to` *)
        match ty with
        | BaseType (basety, term) ->
            BaseType (basety, alpha_conv_term v_from v_to term)
        | FuncType (v, t_arg, t_result) when v = v_from ->
            let sub ty = substitute_ty ty v_from (Var v_from) in
            FuncType (v_to, sub t_arg, sub t_result)
        | FuncType (v, t_arg, t_result) ->
            let conv ty = alpha_conv_ty v_from v_to ty in
            FuncType (v, conv t_arg, conv t_result)
