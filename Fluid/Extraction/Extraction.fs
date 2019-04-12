namespace FluidTypes.Extraction

module Extraction =
    open FSharp.Compiler.AbstractIL.Internal.Library
    open FSharp.Compiler.SourceCodeServices
    open FluidTypes.Refinements
    open FluidTypes.Errors

    exception UnExtractable of Error

    let counter = ref 0

    let fresh_name () =
        let name = sprintf "_%d" !counter in
        counter := !counter + 1
        name


    let rec extract_type (ty: FSharpType) (names: string list) : Ty =
        match ty with
        | Symbol.FunctionType ->
            let args = ty.GenericArguments in
            let args = List.ofSeq args in
            let rec conv_func_type args names =
                match args, names with
                | [arg], _ -> extract_type arg []
                | arg :: args, names ->
                    let ty_arg = extract_type arg [] in
                    let name, names =
                        match List.tryHead names with
                        | Some name -> name, List.tail names
                        | None -> fresh_name (), names
                    in
                    FuncType (name, ty_arg, conv_func_type args names)
                | _ ->
                    failwith "Extracting a function type without arguments"
            in
            conv_func_type args names
        | _ when ty.HasTypeDefinition ->
            match ty.TypeDefinition.DisplayName with
            | "int" -> mk_basetype TInt
            | "bool" -> mk_basetype TBool
            | _ -> failwithf "Unknown type %A" ty
        | _ ->
            failwith "Don't know how to deal with this type"

    let rec extract_expr (e: FSharpExpr) : Term =
        let ty = extract_type e.Type [] in
        match e with
        | BasicPatterns.Application (func_expr, type_args, arg_exprs) ->
            let func = extract_expr func_expr in
            let args = List.map extract_expr arg_exprs in
            List.fold (fun f a -> App (f, a)) func args
        | BasicPatterns.Call (obj_expr_opt, member_or_func, type_args1, type_args2, arg_exprs) ->
            let func =
                match member_or_func.FullName with
                | "Microsoft.FSharp.Core.Operators.( + )" -> Const (Binop Plus)
                | "Microsoft.FSharp.Core.Operators.( - )" -> Const (Binop Minus)
                | "Microsoft.FSharp.Core.Operators.( < )" -> Const (Binop Less)
                | "Microsoft.FSharp.Core.Operators.( > )" -> Const (Binop Greater)
                | "Microsoft.FSharp.Core.Operators.( = )" -> Const (Binop EqualInt) (* FIXME *)
                | "Microsoft.FSharp.Core.Operators.( <> )" -> Const (Binop NotEqualInt) (* FIXME *)
                | "Microsoft.FSharp.Core.Operators.( <= )" -> Const (Binop LessEqual)
                | "Microsoft.FSharp.Core.Operators.( >= )" -> Const (Binop GreaterEqual)
                | "Microsoft.FSharp.Core.Operators.( ~- )" -> Const (Unop Negate)
                | "Microsoft.FSharp.Core.Operators.not" -> Const (Unop Not)
                | name -> Var name
            in
            let obj_opt = Option.map extract_expr obj_expr_opt in
            let args = List.map extract_expr arg_exprs in
            let func =
                match obj_opt with
                | Some o -> App (func, o)
                | None -> func
            in
            List.fold (fun f a -> App (f, a)) func args
        | BasicPatterns.IfThenElse (guard_expr, then_expr, else_expr) ->
            let cond = extract_expr guard_expr in
            let then_ = extract_expr then_expr in
            let else_ = extract_expr else_expr in
            IfThenElse (cond, then_, else_)
        | BasicPatterns.Lambda (lambda_var, body_expr) ->
            let body = extract_expr body_expr in
            Abs (lambda_var.FullName, body)
        | BasicPatterns.Const (const_value_obj, const_type) ->
            match const_value_obj with
            | :?int -> Const (IntLiteral (const_value_obj :?> int))
            | :?bool -> Const (BoolLiteral (const_value_obj :?> bool))
            | otherwise -> UnExtractable (ExtractionError (otherwise.ToString())) |> raise
        | BasicPatterns.Value (value_to_get) ->
            Var value_to_get.FullName
        | otherwise -> UnExtractable (ExtractionError (otherwise.ToString())) |> raise

    let check_terms_in_decls (fileContents : FSharpImplementationFileContents) =
        let decl = fileContents.Declarations in
        let rec check_term (ctx: TyCtx) (decl : FSharpImplementationFileDeclaration) : Error list * TyCtx =
            match decl with
            | FSharpImplementationFileDeclaration.Entity (_entity, decls) ->
                let errors, ctx = List.mapFold check_term ctx decls in
                List.concat errors, ctx
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (member_or_func, arguments, e) ->
                let e = extract_expr e in
                let arguments = List.concat arguments
                                |> List.filter (fun argument -> argument.IsValue)
                in
                let argument_names = List.map (fun (arg : FSharpMemberOrFunctionOrValue) -> arg.FullName) arguments in
                let ty = Option.map (fun ty -> extract_type ty argument_names) member_or_func.FullTypeSafe in
                let e = List.foldBack (fun arg -> fun e -> Abs (arg, e)) argument_names e in
                let errors, ctx =
                    match ty with
                    | Some ty ->
                        if Typing.check_type ctx e ty
                        then [], Typing.env_add_var member_or_func.FullName ty ctx
                        else [TypeError (sprintf "Incorrect Type %A for %A" ty e)], ctx
                    | None ->
                        match Typing.infer_type ctx e with
                        | Some ty -> [], Typing.env_add_var member_or_func.FullName ty ctx
                        | None -> [TypeError (sprintf "Cannot infer type for %A" e)], ctx
                in
                errors, ctx
            | FSharpImplementationFileDeclaration.InitAction _ ->
                [], ctx
        in
        let check_term' ctx decl = try check_term ctx decl with | UnExtractable error -> [error], ctx in
        let ctx = Typing.empty_ctx in
        List.mapFold check_term' ctx decl |> fst
