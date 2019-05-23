namespace FluidTypes.Refinements

module Encoding =
    type EncodingOptions =
        { remove_redundant_terms : bool }

    let default_options : EncodingOptions = { remove_redundant_terms = false }
    let opt = ref default_options

    let empty_env : EncodingEnv =
        { consts = Map.empty
          functions = Map.empty
          clauses = Set.empty }

    let encode_term (env : EncodingEnv) (ctx : TyCtx) (term : Term) : EncodingEnv =
        { env with clauses = Set.add term env.clauses }

    let bind_record_ty (r : RecordDef) (x : Variable) (ty : Ty) : Ty =
        let fields_to_bind = List.map fst r
        let bind_field ty field =
            let bound_field = Var (sprintf "%s$%s" x field)
            Substitution.substitute_ty ty field bound_field
        List.fold bind_field ty fields_to_bind

    let rec encode_ctx_var (env : EncodingEnv) (ctx : TyCtx) (x : Variable) (ty : Ty) : EncodingEnv =
        match ty with
        | BaseType(b, term) ->
            let env =
                encode_term env ctx
                    (Substitution.substitute_term term special_this (Var x))
            { env with consts = Map.add x b env.consts }
        | FuncType _ -> env
        | UnknownType _ -> env
        | UnionType _ -> env
        | RecordType r ->
            match Map.tryFind r ctx.recordDef with
            | Some defs ->
                let encode_flatten env (name, ty) =
                    encode_ctx_var env ctx (sprintf "%s$%s" x name) (bind_record_ty defs x ty)
                List.fold encode_flatten env defs
            | None -> failwithf "Internal error: Missing record definition for %s" r
        | ProductType tys ->
            let encode_flatten env (idx, ty) =
                encode_ctx_var env ctx (sprintf "%s$%d" x idx) ty
            List.fold encode_flatten env (List.indexed tys)

    let encode_ctx (env : EncodingEnv) (ctx : TyCtx) : EncodingEnv =
        let var_ctx = ctx.varCtx
        let env = Map.fold (fun e -> encode_ctx_var e ctx) env var_ctx
        { env with clauses =
                       List.fold (fun cls p -> Set.add p cls) env.clauses
                           ctx.predicateCtx }

    let check_subtype (ctx : TyCtx) (term_1 : Term) (term_2 : Term)
        (base_type : BaseTy) : bool =
        let env = empty_env
        let env =
            { env with consts = Map.add special_this base_type env.consts }
        let env = encode_ctx env ctx
        let env = encode_term env ctx term_1
        let env = encode_term env ctx (mk_not term_2)
        Solver.solve_encoding env
