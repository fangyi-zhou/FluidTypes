open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO

let countDecl (fileContents : FSharpImplementationFileContents) =
    let decl = fileContents.Declarations in
    let rec checkDecl (decl : FSharpImplementationFileDeclaration) : int =
        match decl with
        | FSharpImplementationFileDeclaration.Entity (_entity, decls) ->
            printf "Entity\n";
            List.sumBy checkDecl decls
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue _ ->
            printf "MemberOrFunctionOrValue";
            1
        | FSharpImplementationFileDeclaration.InitAction _ ->
            printf "InitAction";
            1
    in
    let declCounts = List.map checkDecl decl in
    List.sum declCounts

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
        let declCounts = List.map countDecl results.AssemblyContents.ImplementationFiles in
        printf "%d Declarations in the file(s)\n" (List.sum declCounts)
        0
