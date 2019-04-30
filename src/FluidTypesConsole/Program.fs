open Microsoft.FSharp.Compiler.SourceCodeServices
open FluidTypes.Extraction
open Microsoft.FSharp.Core
open System.IO
open Argu

type CliArguments =
    | Log_Smt_Queries
    | Z3_Path of string
    | [<MainCommand; Mandatory>] Input of string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Log_Smt_Queries ->
                "Whether or not to store SMT queries in log files"
            | Z3_Path _ -> "Specify path to Z3"
            | Input _ -> "Specify a file to check (.fsproj or .fsx)"

let handle_args (results : ParseResults<CliArguments>) =
    if results.Contains Z3_Path then
        (let z3_path = results.GetResult Z3_Path
         FluidTypes.Refinements.Solver.set_z3_path z3_path)
    if results.Contains Log_Smt_Queries then
        FluidTypes.Refinements.Solver.set_log_queries true
    results.GetResult Input

let process_input (filename : string) =
    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    let projectOptions, errors =
        match Path.GetExtension filename with
        | ".fsx" ->
            checker.GetProjectOptionsFromScript(filename, filename)
            |> Async.RunSynchronously
        | ".fs" -> failwith "Unimplemented"
        | ".fsproj" ->
            ProjectCracker.GetProjectOptionsFromProjectFile(filename), []
        | ext -> failwithf "Unsupported File Extension %s" ext

    let results =
        checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    if Array.isEmpty results.Errors then
        (let errors =
             List.map Extraction.check_terms_in_decls
                 results.AssemblyContents.ImplementationFiles
         if List.isEmpty errors then 0
         else
             printfn "FluidTypes Errors %A" errors
             1)
    else
        printfn "Input contains compilation errors:"
        printfn "%A" results.Errors
        1

[<EntryPoint>]
let main argv =
    let error_handler = ProcessExiter()
    let parser =
        ArgumentParser.Create<CliArguments>
            (errorHandler = error_handler, programName = "Fluid.exe")
    let results = parser.Parse()
    let filename = handle_args results
    process_input filename
