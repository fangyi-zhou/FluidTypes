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
        (* TODO *)
        env

    let encode_ctx_var (opt: EncodingOptions) (env: EncodingEnv) (x: Variable) (ty: Ty) : EncodingEnv =
        (* TODO *)
        env

    let encode_ctx (opt: EncodingOptions) (env: EncodingEnv) (ctx: TyCtx) : EncodingEnv =
        let var_ctx = ctx.varCtx in
        let env = Map.fold (encode_ctx_var opt) env var_ctx in
        {env with clauses = List.fold (fun cls p -> Set.add p cls) env.clauses ctx.predicateCtx}

    let check_subtype (opt: EncodingOptions) (ctx: TyCtx) (term_1: Term) (term_2: Term) : bool =
        let env = empty_env in
        let env = encode_ctx opt env ctx in
        let env = encode_term opt env term_1 in
        let env = encode_term opt env (mk_not term_2) in
        Solver.solve_encoding env