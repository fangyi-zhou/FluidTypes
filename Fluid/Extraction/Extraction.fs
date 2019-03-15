namespace FluidTypes.Extraction

module Extraction =
    open FSharp.Compiler.SourceCodeServices
    open FluidTypes.Refinements

    exception UnExtractable

    let rec extract_expr (e: FSharpExpr) : unit =
        let () =
            match e with
            | BasicPatterns.Application (func_expr, type_args, arg_exprs) ->
                printf "Application %A %A\n" func_expr arg_exprs;
                let _ = extract_expr func_expr in
                let _ = List.map extract_expr arg_exprs in
                ()
            | BasicPatterns.Call (obj_expr_opt, member_or_func, type_args1, type_args2, arg_exprs) ->
                printf "Call %A %A %A\n" obj_expr_opt member_or_func arg_exprs;
                let _ = Option.iter extract_expr obj_expr_opt in
                let _ = List.map extract_expr arg_exprs in
                ()
            | BasicPatterns.IfThenElse (guard_expr, then_expr, else_expr) ->
                printf "IfThenElse %A %A %A\n" guard_expr then_expr else_expr;
                let _ = extract_expr guard_expr in
                let _ = extract_expr then_expr in
                let _ = extract_expr else_expr in
                ()
            | BasicPatterns.Lambda (lambda_var, body_expr) ->
                printf "Lambda %A %A\n" lambda_var body_expr
                let _ = extract_expr body_expr in
                ()
            | BasicPatterns.BaseValue base_type ->
                printf "BaseValue %A \n" base_type
                ()
            | BasicPatterns.Const (const_value_obj, const_type) ->
                printf "Const %A %A \n" const_value_obj const_type
                ()
            | BasicPatterns.Value (value_to_get) ->
                printf "Value %A \n" value_to_get
                ()
            | _ -> ()
        in
        ()


    let countDecl (fileContents : FSharpImplementationFileContents) =
        let decl = fileContents.Declarations in
        let rec checkDecl (decl : FSharpImplementationFileDeclaration) : int =
            match decl with
            | FSharpImplementationFileDeclaration.Entity (_entity, decls) ->
                List.sumBy checkDecl decls
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (_, _, e) ->
                extract_expr e;
                1
            | FSharpImplementationFileDeclaration.InitAction _ ->
                1
        in
        let declCounts = List.map checkDecl decl in
        List.sum declCounts
