namespace FluidTypes

module Solver =
    open System.Diagnostics

    type SolverOptions = {
        path: string;
        options: string;
    }

    let default_options: SolverOptions = {
        path = "z3";
        options = "";
    }

    let run_solver (opt: SolverOptions) (formula : string) : string =
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
        let prepend ref_l v = ref_l := v :: !ref_l in
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
        List.foldBack (fun acc v -> acc + "\n" + v) !outputs ""

    let solve_encoding (env: EncodingEnv) : bool = (* TODO *) false