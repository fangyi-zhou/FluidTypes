namespace FluidTypes.Refinements

module Solver =
    open System.IO
    open System
    open System.Runtime.InteropServices
    open System.Diagnostics

    type SolverOptions =
        { path : string option
          options : string
          log_queries : bool }

    let find_z3() =
        let path = Environment.GetEnvironmentVariable "PATH"
        let directory = path.Split Path.PathSeparator

        let filename =
            if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform
                   (OSPlatform.Windows) then "z3.exe"
            else "z3"

        let file_exist dir = File.Exists(Path.Combine(dir, filename))
        let full_paths = Array.filter file_exist directory
        if full_paths.Length = 0 then failwith "Z3 not found"
        else Path.Combine(full_paths.[0], filename)

    let default_options : SolverOptions =
        { path = None
          options = ""
          log_queries = false }

    let opt = ref default_options
    let set_z3_path z3_path = opt := { !opt with path = Some z3_path }
    let set_log_queries log_queries =
        opt := { !opt with log_queries = log_queries }

    exception UnEncodable

    let run_solver (formula : string) : string list =
        if (!opt).path.IsNone then set_z3_path (find_z3())
        let opt = !opt
        let path = Option.get opt.path
        let time = System.DateTime.Now.ToBinary()

        let log_file_writer =
            if opt.log_queries then
                let filename = (sprintf "%u.log" time)
                let writer = new System.IO.StreamWriter(filename)
                Some writer
            else None

        let log (content : string) =
            Option.iter (fun (writer : StreamWriter) ->
                writer.Write(content)
                writer.WriteLine()) log_file_writer

        let tempfile = System.IO.Path.GetTempFileName()
        let writer = new System.IO.StreamWriter(tempfile)
        writer.Write(formula)
        writer.Close()
        log formula
        (* http://www.fssnip.net/sw/title/RunProcess *)
        let proc_info =
            ProcessStartInfo(RedirectStandardOutput = true,
                             RedirectStandardError = true,
                             UseShellExecute = false, FileName = path,
                             Arguments = if opt.options <> "" then
                                             (tempfile + " " + opt.options)
                                         else tempfile)

        let outputHandler f (_sender : obj) (args : DataReceivedEventArgs) =
            f args.Data
        let outputs = ref []
        let errors = ref []

        let prepend ref_l v =
            if v <> "" then ref_l := v :: !ref_l

        let p = new Process(StartInfo = proc_info)
        p.OutputDataReceived.AddHandler
            (DataReceivedEventHandler(outputHandler (prepend outputs)))
        p.ErrorDataReceived.AddHandler
            (DataReceivedEventHandler(outputHandler (prepend errors)))
        let started =
            try
                p.Start()
            with ex ->
                ex.Data.Add("filename", path)
                reraise()
        if not started then failwithf "Failed to start process %s" path
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()
        log (String.concat "\n" !outputs)
        log (String.concat "\n" !errors)
        Option.iter (fun (writer : StreamWriter) -> writer.Close())
            log_file_writer
        !outputs

    let is_unsat (formula : string) : bool =
        let result = run_solver formula
        List.exists (fun v -> v = "unsat") result

    let solve_encoding (env : EncodingEnv) : bool =
        let smt_script = ref []
        let prepend ref_l v = ref_l := v :: !ref_l

        let show_ty ty =
            match ty with
            | TBool -> "Bool"
            | TInt -> "Int"

        let add_const var ty =
            prepend smt_script
                (sprintf "(declare-const %s %s)\n" var (show_ty ty))

        Map.iter add_const env.consts
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

        let unop_sexp unop =
            match unop with
            | Not -> "not"
            | Negate -> "-"

        let rec term_to_sexp term =
            match term with
            | Var x -> x
            | Const(IntLiteral i) -> sprintf "%d" i
            | Const(BoolLiteral b) ->
                if b then "true"
                else "false"
            | App(App(Const(Binop binop), term_1), term_2) ->
                let sexp_1 = term_to_sexp term_1
                let sexp_2 = term_to_sexp term_2
                sprintf "(%s %s %s)" (binop_sexp binop) sexp_1 sexp_2
            | App(Const(Unop unop), term_) ->
                let sexp_ = term_to_sexp term_
                sprintf "(%s %s)" (unop_sexp unop) sexp_
            | IfThenElse(term_cond, term_then, term_else) ->
                let sexp_cond = term_to_sexp term_cond
                let sexp_then = term_to_sexp term_then
                let sexp_else = term_to_sexp term_else
                sprintf "(ite %s %s %s)" sexp_cond sexp_then sexp_else
            | FieldGet(Var v, field) ->
                sprintf "%s$%s" v field
            | _ -> raise UnEncodable

        let encode_term term =
            try
                prepend smt_script (sprintf "(assert %s)" (term_to_sexp term))
            with UnEncodable ->
                printfn "Unencodable %A" term
                ()

        Set.iter encode_term env.clauses
        prepend smt_script "(check-sat)"
        let script =
            String.concat "\n" (List.rev !smt_script)
        is_unsat script
