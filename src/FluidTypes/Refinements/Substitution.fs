namespace FluidTypes.Refinements

module Substitution =
    let find_replacement_var (v : Variable) (excludes : Set<Variable>) : Variable =
        let rec aux i =
            let var = sprintf "%s_%d" v i
            if Set.contains var excludes then aux (i + 1)
            else var
        aux 0

    let rec substitute_term (term : Term) (x : Variable) (replace : Term) : Term =
        let sub term = substitute_term term x replace
        match term with
        | Var v when x = v -> replace
        | Var v -> term
        | Const c -> term
        | Abs(v, _) when x = v -> term
        | Abs(v, term_) ->
            let free_vars = FreeVar.free_var_term replace
            if Set.contains v free_vars then
                sub (alpha_conv_term v (find_replacement_var v free_vars) term)
            else Abs(v, sub term_)
        | App(term_1, term_2) -> App(sub term_1, sub term_2)
        | IfThenElse(term_cond, term_then, term_else) ->
            IfThenElse(sub term_cond, sub term_then, sub term_else)
        | Anno(term_, ty) -> Anno(sub term_, substitute_ty ty x replace)
        | Coerce(term_, ty) -> Coerce(sub term_, substitute_ty ty x replace)
        | Let(v, t1, t2) ->
            let free_vars = FreeVar.free_var_term replace
            if Set.contains v free_vars then
                sub (alpha_conv_term v (find_replacement_var v free_vars) term)
            else Let(v, sub t1, sub t2)
        | UnknownTerm _ -> term
        | FieldGet(term, field) -> FieldGet(sub term, field)
        | NewRecord(terms, record) -> NewRecord(List.map sub terms, record)
        | Tuple(terms) -> Tuple(List.map sub terms)
        | Diverge -> Diverge

    and substitute_ty (ty : Ty) (x : Variable) (replace : Term) : Ty =
        let sub ty = substitute_ty ty x replace
        match ty with
        | BaseType(basety, term) ->
            BaseType(basety, substitute_term term x replace)
        | FuncType(v, _, _) when x = v -> ty
        | FuncType(v, t_arg, t_result) ->
            let free_vars = FreeVar.free_var_term replace
            if Set.contains v free_vars then
                sub (alpha_conv_ty v (find_replacement_var v free_vars) ty)
            else FuncType(v, sub t_arg, sub t_result)
        | UnknownType _ -> ty
        | RecordType _ -> ty
        | UnionType _ -> ty
        | ProductType tys -> ProductType(List.map sub tys)

    and alpha_conv_term (v_from : Variable) (v_to : Variable) (term : Term) : Term =
        (* Replace all bound `v_from` to `v_to` *)
        let conv term = alpha_conv_term v_from v_to term
        match term with
        | Var v when v = v_from -> Var v_to
        | Var v -> Var v
        | Const c -> Const c
        | Abs(v, term_) when v = v_from ->
            Abs(v_to, substitute_term term_ v (Var v_to))
        | Abs(v, term_) -> Abs(v, conv term_)
        | App(term_1, term_2) -> App(conv term_1, conv term_2)
        | IfThenElse(term_cond, term_then, term_else) ->
            IfThenElse(conv term_cond, conv term_then, conv term_else)
        | Anno(term_, ty) -> Anno(conv term_, alpha_conv_ty v_from v_to ty)
        | Coerce(term_, ty) -> Coerce(conv term_, alpha_conv_ty v_from v_to ty)
        | Let(v, t1, t2) when v = v_from ->
            Let(v_to, conv t1, substitute_term t2 v (Var v_to))
        | Let(v, t1, t2) ->
            Let(v, conv t1, conv t2)
        | FieldGet(term, field) -> FieldGet(conv term, field)
        | NewRecord(terms, record) -> NewRecord(List.map conv terms, record)
        | Tuple(terms) -> Tuple(List.map conv terms)
        | UnknownTerm(u, ty) -> UnknownTerm(u, ty)
        | Diverge -> Diverge

    and alpha_conv_ty (v_from : Variable) (v_to : Variable) (ty : Ty) : Ty =
        (* Replace all bound `v_from` to `v_to` *)
        let conv ty = alpha_conv_ty v_from v_to ty
        match ty with
        | BaseType(basety, term) ->
            BaseType(basety, alpha_conv_term v_from v_to term)
        | FuncType(v, t_arg, t_result) when v = v_from ->
            let sub ty = substitute_ty ty v_from (Var v_to)
            FuncType(v_to, sub t_arg, sub t_result)
        | FuncType(v, t_arg, t_result) ->
            FuncType(v, conv t_arg, conv t_result)
        | UnknownType _ -> ty
        | RecordType _ -> ty
        | UnionType _ -> ty
        | ProductType tys -> ProductType(List.map conv tys)

    and resolve_unknown_ty_in_ty (name : string) (typedef : Ty) (ty_to_resolve : Ty) : Ty =
        let resolve = resolve_unknown_ty_in_ty name typedef
        match ty_to_resolve with
        | BaseType(basety, term) ->
            BaseType(basety, resolve_unknown_ty_in_term name typedef term)
        | FuncType(v, t_arg, t_result) ->
            FuncType(v, resolve t_arg, resolve t_result)
        | UnknownType name_ when name = name_ -> typedef
        | UnknownType name_ -> UnknownType name_
        | RecordType r -> RecordType r
        | UnionType u -> UnionType u
        | ProductType tys -> ProductType (List.map resolve tys)

    and resolve_unknown_ty_in_term (name : string) (typedef : Ty) (term_to_resolve : Term) : Term =
        let resolve = resolve_unknown_ty_in_term name typedef
        match term_to_resolve with
        | Var x -> Var x
        | Const c -> Const c
        | App (t1, t2) -> App (resolve t1, resolve t2)
        | Abs (v, t) -> Abs (v, resolve t)
        | IfThenElse (t1, t2, t3) -> IfThenElse (resolve t1, resolve t2, resolve t3)
        | Anno (t, ty) -> Anno (resolve t, resolve_unknown_ty_in_ty name typedef ty)
        | Coerce (t, ty) -> Coerce (resolve t, resolve_unknown_ty_in_ty name typedef ty)
        | Let(v, t1, t2) -> Let (v, resolve t1, resolve t2)
        | UnknownTerm (s, ty) -> UnknownTerm (s, resolve_unknown_ty_in_ty name typedef ty)
        | NewRecord (terms, s) -> NewRecord (List.map resolve terms, s)
        | FieldGet (term, f) -> FieldGet (resolve term, f)
        | Tuple terms -> Tuple(List.map resolve terms)
        | Diverge -> Diverge
