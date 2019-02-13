open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Core
open System.IO

let rec checkExpr (e: FSharpExpr) =
    let () =
        match e with
        | BasicPatterns.Const (o, ty) ->
            match ty with
            | int -> printf "%d\n" (o :?> int)
            | _ -> ()
        | _ -> ()
    in
    List.iter checkExpr e.ImmediateSubExpressions

let countDecl (fileContents : FSharpImplementationFileContents) =
    let decl = fileContents.Declarations in
    let rec checkDecl (decl : FSharpImplementationFileDeclaration) : int =
        match decl with
        | FSharpImplementationFileDeclaration.Entity (_entity, decls) ->
            List.sumBy checkDecl decls
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (_, _, e) ->
            checkExpr e;
            1
        | FSharpImplementationFileDeclaration.InitAction _ ->
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
