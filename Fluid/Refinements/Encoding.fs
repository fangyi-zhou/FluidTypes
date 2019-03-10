namespace FluidTypes

module Encoding =

    type EncodingOptions = {
        remove_redundant_terms: bool;
    }

    let default_options : EncodingOptions = {
        remove_redundant_terms = false;
    }

    let empty_env : EncodingEnv = {
        consts = Map.empty;
        functions = Map.empty;
        clauses = Set.empty;
    }

    let encode_term (opt: EncodingOptions) (env: EncodingEnv) (term: Term) : EncodingEnv =
        {env with clauses = Set.add term env.clauses}

    let encode_ctx_var (opt: EncodingOptions) (env: EncodingEnv) (x: Variable) (ty: Ty) : EncodingEnv =
        match ty with
        | BaseType (b, term) ->
            let env = encode_term opt env (Substitution.substitute_term (Var x) special_this term)
            {env with consts = Map.add x b env.consts }
        | FuncType _ -> env

    let encode_ctx (opt: EncodingOptions) (env: EncodingEnv) (ctx: TyCtx) : EncodingEnv =
        let var_ctx = ctx.varCtx in
        let env = Map.fold (encode_ctx_var opt) env var_ctx in
        {env with clauses = List.fold (fun cls p -> Set.add p cls) env.clauses ctx.predicateCtx}

    let check_subtype (opt: EncodingOptions) (ctx: TyCtx) (term_1: Term) (term_2: Term) (base_type: BaseTy) : bool =
        let env = empty_env in
        let env = {env with consts = Map.add special_this base_type env.consts} in
        let env = encode_ctx opt env ctx in
        let env = encode_term opt env term_1 in
        let env = encode_term opt env (mk_not term_2) in
        Solver.solve_encoding env
