namespace FluidTypes.Refinements

module FreeVar =
    let rec free_var_term (term : Term) : Set<Variable> =
        match term with
        | Var v -> Set.singleton v
        | Const _ -> Set.empty
        | App(term_1, term_2) ->
            Set.union (free_var_term term_1) (free_var_term term_2)
        | Abs(v, term_) -> Set.remove v (free_var_term term_)
        | IfThenElse(term_cond, term_then, term_else) ->
            Set.unionMany [ free_var_term term_cond
                            free_var_term term_then
                            free_var_term term_else ]
        | Anno(term_, ty)
        | Coerce(term_, ty) -> Set.union (free_var_term term_) (free_var_ty ty)
        | Let(var, t1, t2) -> Set.union (free_var_term t1) (Set.remove var (free_var_term t2))
        | FieldGet(term_, _) -> free_var_term term_
        | NewRecord(terms, _) ->
            List.map free_var_term terms |> Set.unionMany
        | UnknownTerm _ -> Set.empty
        | Tuple(terms) -> Set.unionMany (List.map free_var_term terms)
        | Diverge -> Set.empty

    and free_var_ty (ty : Ty) : Set<Variable> =
        match ty with
        | BaseType(_, term) -> Set.remove special_this (free_var_term term)
        | FuncType(v, t_arg, t_result) ->
            Set.union (free_var_ty t_arg) (Set.remove v (free_var_ty t_result))
        | UnknownType _ -> Set.empty
        | RecordType _ -> Set.empty
        | UnionType _ -> Set.empty
        | ProductType tys -> Set.unionMany (List.map free_var_ty tys)
