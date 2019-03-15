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
        printf "%d Error(s)\n" (Array.length results.Errors)
        let declCounts = List.map Extraction.countDecl results.AssemblyContents.ImplementationFiles in
        printf "%d Declarations in the file(s)\n" (List.sum declCounts)
        0
