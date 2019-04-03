namespace FluidTypes.Refinements

module Solver =
    open System.IO
    open System
    open System.Runtime.InteropServices
    open System.Diagnostics

    type SolverOptions = {
        path: string;
        options: string;
    }

    let find_z3 () =
        let path = Environment.GetEnvironmentVariable "PATH" in
        let directory = path.Split Path.PathSeparator in
        let filename =
            if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
            then "z3.exe"
            else "z3"
        in
        let file_exist dir = File.Exists(Path.Combine (dir, filename)) in
        let full_paths = Array.filter file_exist directory
        if full_paths.Length = 0
        then failwith "Z3 not found"
        else Path.Combine (full_paths.[0], filename)

    let default_options: SolverOptions = {
        path = find_z3 ();
        options = "";
    }

    exception UnEncodable

    let run_solver (opt: SolverOptions) (formula : string) : string list =
        let tempfile = System.IO.Path.GetTempFileName () in
        let writer = new System.IO.StreamWriter (tempfile) in
        writer.Write (formula);
        writer.Close ();
        (* http://www.fssnip.net/sw/title/RunProcess *)
        let proc_info =
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = opt.path,
                Arguments = if opt.options <> "" then (tempfile + " " + opt.options) else tempfile
        )
        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data in
        let outputs = ref [] in
        let errors = ref [] in
        let prepend ref_l v = if v <> "" then ref_l := v :: !ref_l in
        let p = new Process(StartInfo = proc_info) in
        p.OutputDataReceived.AddHandler (DataReceivedEventHandler (outputHandler (prepend outputs)));
        p.ErrorDataReceived.AddHandler (DataReceivedEventHandler (outputHandler (prepend errors)));
        let started =
            try
                p.Start ()
            with | ex ->
                ex.Data.Add ("filename", opt.path)
                reraise ()
        if not started then
            failwithf "Failed to start process %s" opt.path
        p.BeginOutputReadLine ();
        p.BeginErrorReadLine ();
        p.WaitForExit ();
        !outputs

    let is_unsat (opt: SolverOptions) (formula : string) : bool =
        let result = run_solver opt formula in
        List.exists (fun v -> v = "unsat") result

    let solve_encoding (env: EncodingEnv) : bool =
        let smt_script = ref [] in
        let prepend ref_l v = ref_l := v :: !ref_l in
        let show_ty ty =
            match ty with
            | TBool -> "Bool"
            | TInt -> "Int"
        in
        let add_const var ty =
            prepend smt_script (sprintf "(declare-const %s %s)\n" var (show_ty ty))
        in
        Map.iter add_const env.consts;
        let binop_sexp binop =
            match binop with
            | Plus -> "+"
            | Minus -> "-"
            | And -> "and"
            | Or -> "or"
            | EqualInt -> "="
            | NotEqualInt -> raise UnEncodable
            | EqualBool -> "="
            | NotEqualBool -> raise UnEncodable
            | Greater -> ">"
            | GreaterEqual -> ">="
            | Less -> "<"
            | LessEqual -> "<="
        in
        let unop_sexp unop =
            match unop with
            | Not -> "not"
            | Negate -> "-"
        in
        let rec term_to_sexp term =
            match term with
            | Var x -> x
            | Const (IntLiteral i) -> sprintf "%d" i
            | Const (BoolLiteral b) -> if b then "true" else "false"
            | App (App (Const (Binop binop), term_1), term_2) ->
                let sexp_1 = term_to_sexp term_1 in
                let sexp_2 = term_to_sexp term_2 in
                sprintf "(%s %s %s)" (binop_sexp binop) sexp_1 sexp_2
            | App (Const (Unop unop), term_) ->
                let sexp_ = term_to_sexp term_ in
                sprintf "(%s %s)" (unop_sexp unop) sexp_
            | IfThenElse (term_cond, term_then, term_else) ->
                let sexp_cond = term_to_sexp term_cond in
                let sexp_then = term_to_sexp term_then in
                let sexp_else = term_to_sexp term_else in
                sprintf "(ite %s %s %s)" sexp_cond sexp_then sexp_else
            | _ -> raise UnEncodable
        in
        let encode_term term =
            try
                prepend smt_script (sprintf "(assert %s)" (term_to_sexp term))
            with UnEncodable ->
                ()
        in
        Set.iter encode_term env.clauses;
        prepend smt_script "(check-sat)";
        let script = String.concat "\n" (List.rev !smt_script) in
        is_unsat default_options script
