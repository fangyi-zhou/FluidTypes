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

    let rec encode_ctx_var (env : EncodingEnv) (ctx : TyCtx) (x : Variable) (ty : Ty) : EncodingEnv =
        match ty with
        | BaseType(b, term) ->
            let env =
                encode_term env ctx
                    (Substitution.substitute_term term x (Var special_this))
            { env with consts = Map.add x b env.consts }
        | FuncType _ -> env
        | UnknownType _ -> env
        | RecordType r ->
            match Map.tryFind r ctx.recordDef with
            | Some defs ->
                let encode_flatten env (name, ty) =
                    encode_ctx_var env ctx (sprintf "%s#%s" x name) ty
                List.fold encode_flatten env defs
            | None -> failwithf "Internal error: Missing record definition for %s" r
        | ProductType tys -> failwith "TODO"

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
