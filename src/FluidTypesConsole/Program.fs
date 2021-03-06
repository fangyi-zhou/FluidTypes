open FSharp.Compiler.SourceCodeServices
open Dotnet.ProjInfo.Workspace
open Dotnet.ProjInfo.Workspace.FCS
open FluidTypes.Errors
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

let get_project_options filename checker =
    let msbuild_locator = MSBuildLocator ()
    let loader_config = LoaderConfig.Default (msbuild_locator)
    let loader = Loader.Create(loader_config)
    let filename = Path.GetFullPath(filename)
    loader.LoadProjects [filename]
    let netfw_info_config = NetFWInfoConfig.Default (msbuild_locator)
    let netfw_info = NetFWInfo.Create (netfw_info_config)
    let fcs_binder = FCSBinder (netfw_info, loader, checker)
    fcs_binder.GetProjectOptions (filename)

let process_input (filename : string) =
    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    let projectOptions, errors =
        match Path.GetExtension filename with
        | ".fsx" ->
            checker.GetProjectOptionsFromScript(filename, filename)
            |> Async.RunSynchronously
        | ".fs" -> failwith "Unimplemented"
        | ".fsproj" ->
            match get_project_options filename checker with
            | Some options -> options, []
            | None -> failwith "Unable to load project options"
        | ext -> failwithf "Unsupported File Extension %s" ext

    let results =
        checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
    if Array.isEmpty results.Errors then begin
        Extraction.check_impl_files results.AssemblyContents.ImplementationFiles
        report_errors()
    end
    else
        printfn "Input contains compilation errors:"
        Seq.iter (printfn "%A") results.Errors
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
