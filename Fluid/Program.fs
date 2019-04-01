open FSharp.Compiler.SourceCodeServices
open FluidTypes.Extraction
open Microsoft.FSharp.Core
open System.IO

[<EntryPoint>]
let main argv =
    let checker = FSharpChecker.Create(keepAssemblyContents=true) in
    if Array.isEmpty argv
    then
        fprintfn stderr "%s" "No arguments provided"
        1
    else
        let filename = argv.[0] in
        let projectOptions, errors =
            match Path.GetExtension filename with
            | ".fsx" -> checker.GetProjectOptionsFromScript(filename, filename) |> Async.RunSynchronously
            | ".fs" -> failwith "Unimplemented"
            | ext -> failwithf "Unsupported File Extension %s" ext
        in
        let results = checker.ParseAndCheckProject(projectOptions) |> Async.RunSynchronously
        in
        if Array.isEmpty results.Errors
        then begin
            let errors = List.map Extraction.check_terms_in_decls results.AssemblyContents.ImplementationFiles in
            if List.isEmpty errors
            then 0
            else
                printfn "FluidTypes Errors %A" errors;
                1
            end
        else
            printfn "Input contains compilation errors:"
            printfn "%A" results.Errors
            1
