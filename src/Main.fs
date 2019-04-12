namespace FluidTypes

module Main =

    open FSharp.Compiler.SourceCodeServices
    open FluidTypes.Extraction
    open Microsoft.FSharp.Core
    open System.IO

    (* FIXME: Remove this massive hack *)
    let fixProjectOptions (projectOptions: FSharpProjectOptions) =
      { projectOptions
        with OtherOptions
          = Array.append projectOptions.OtherOptions [|"-r:/usr/local/share/dotnet/shared/Microsoft.NETCore.App/2.1.6/System.Runtime.dll"|]
      }

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
                | ".fsx" -> checker.GetProjectOptionsFromScript(filename, filename, assumeDotNetFramework = false) |> Async.RunSynchronously
                | ".fs" -> failwith "Unimplemented"
                | ext -> failwithf "Unsupported File Extension %s" ext
            in
            let projectOptions = fixProjectOptions projectOptions in
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
