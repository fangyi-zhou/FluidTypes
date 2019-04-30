namespace FluidTypes.Extraction

open System.Reflection.Metadata

module Extraction =
    open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FluidTypes.Refinements
    open FluidTypes.Annotations.AnnotationParser
    open FluidTypes.Errors

    exception UnExtractable of Error

    type TyMap = Map<string, Ty>

    type ExtractionCtx =
        { ty_ctx : TyCtx
          ty_map : TyMap }

    let default_ty_map : TyMap =
        Map.ofList [ "int", mk_basetype TInt
                     "bool", mk_basetype TBool ]

    let default_ctx : ExtractionCtx =
        { ty_ctx = Typing.empty_ctx
          ty_map = default_ty_map }

    let counter = ref 0

    let fresh_name() =
        let name = sprintf "_%d" !counter
        counter := !counter + 1
        name

    let is_refined_attribute (attribute : FSharpAttribute) =
        let ty = attribute.AttributeType
        ty.FullName = "FluidTypes.Annotations.RefinedAttribute"

    let find_refined_attribute (attributes : FSharpAttribute seq) =
        Seq.tryHead (Seq.filter is_refined_attribute attributes)

    let attribute_to_ty (attribute : FSharpAttribute) =
        let refinement = attribute.ConstructorArguments.[0] |> snd :?> string
        parse_ty refinement

    let rec extract_type (ctx : ExtractionCtx) (ty : FSharpType)
            (names : string list) : Ty =
        match ty with
        | Symbol.FunctionType ->
            let args = ty.GenericArguments
            let args = List.ofSeq args

            let rec conv_func_type args names =
                match args, names with
                | [ arg ], _ -> extract_type ctx arg []
                | arg :: args, names ->
                    let ty_arg = extract_type ctx arg []

                    let name, names =
                        match List.tryHead names with
                        | Some name -> name, List.tail names
                        | None -> fresh_name(), names
                    FuncType(name, ty_arg, conv_func_type args names)
                | _ -> failwith "Extracting a function type without arguments"
            conv_func_type args names
        | _ when ty.HasTypeDefinition ->
            let ty_map = ctx.ty_map
            let name = Option.attempt (fun () -> ty.TypeDefinition.FullName)
            let name = Option.defaultValue (ty.TypeDefinition.DisplayName) name
            match Map.tryFind name ty_map with
            | Some ty -> ty
            | None ->
                let typeName = name
                printfn "Unknown Type: %s" typeName
                UnknownType typeName
        | _ ->
            let typeName = ty.ToString()
            printfn "Unknown Type %s without definition " typeName
            UnknownType(typeName + fresh_name())

    let rec extract_expr (ctx : ExtractionCtx) (ty : Ty option) (e : FSharpExpr) : Term =
        let ty = extract_type ctx e.Type []
        match e with
        | BasicPatterns.Application(func_expr, type_args, arg_exprs) ->
            (* FIXME: None in ty *)
            let func = extract_expr ctx None func_expr
            (* FIXME: None in ty *)
            let args = List.map (extract_expr ctx None) arg_exprs
            List.fold (fun f a -> App(f, a)) func args
        | BasicPatterns.Call(obj_expr_opt, member_or_func, type_args1,
                             type_args2, arg_exprs) ->
            let func =
                match member_or_func.FullName with
                | "Microsoft.FSharp.Core.Operators.( + )" -> Const(Binop Plus)
                | "Microsoft.FSharp.Core.Operators.( - )" -> Const(Binop Minus)
                | "Microsoft.FSharp.Core.Operators.( < )" -> Const(Binop Less)
                | "Microsoft.FSharp.Core.Operators.( > )" ->
                    Const(Binop Greater)
                | "Microsoft.FSharp.Core.Operators.( = )" ->
                    Const(Binop EqualInt)
                (* FIXME *)
                | "Microsoft.FSharp.Core.Operators.( <> )" ->
                    Const(Binop NotEqualInt)
                (* FIXME *)
                | "Microsoft.FSharp.Core.Operators.( <= )" ->
                    Const(Binop LessEqual)
                | "Microsoft.FSharp.Core.Operators.( >= )" ->
                    Const(Binop GreaterEqual)
                | "Microsoft.FSharp.Core.Operators.( ~- )" -> Const(Unop Negate)
                | "Microsoft.FSharp.Core.Operators.not" -> Const(Unop Not)
                | name -> Var name

            (* FIXME: None in ty *)
            let obj_opt = Option.map (extract_expr ctx None) obj_expr_opt
            let args = List.map (extract_expr ctx None) arg_exprs

            let func =
                match obj_opt with
                | Some o -> App(func, o)
                | None -> func
            List.fold (fun f a -> App(f, a)) func args
        | BasicPatterns.IfThenElse(guard_expr, then_expr, else_expr) ->
            let cond = extract_expr ctx None guard_expr
            let then_ = extract_expr ctx None then_expr
            let else_ = extract_expr ctx None else_expr
            IfThenElse(cond, then_, else_)
        | BasicPatterns.Lambda(lambda_var, body_expr) ->
            let body = extract_expr ctx None body_expr
            Abs(lambda_var.FullName, body)
        | BasicPatterns.Const(const_value_obj, const_type) ->
            match const_value_obj with
            | :? int -> Const(IntLiteral(const_value_obj :?> int))
            | :? bool -> Const(BoolLiteral(const_value_obj :?> bool))
            | otherwise ->
                let const_value = const_value_obj.ToString()
                printfn "Unknown const %s" const_value
                UnknownTerm(const_value, extract_type ctx const_type [])
        | BasicPatterns.Value(value_to_get) -> Var value_to_get.FullName
        | otherwise ->
            let e = e.ToString()
            printfn "Unknown expression %s" e
            UnknownTerm(e, ty)

    let check_terms_in_decls (fileContents : FSharpImplementationFileContents) =
        let decl = fileContents.Declarations

        let rec check_term (ctx : ExtractionCtx)
                (decl : FSharpImplementationFileDeclaration) : Error list * ExtractionCtx =
            match decl with
            | FSharpImplementationFileDeclaration.Entity(entity, decls) ->
                let error_opt, ty_map =
                    if entity.IsFSharpAbbreviation then
                        let ty_abbrev =
                            extract_type ctx entity.AbbreviatedType []
                        let refined_attribute =
                            find_refined_attribute entity.Attributes
                        let refined_ty =
                            Option.map attribute_to_ty refined_attribute
                        let refined_ty =
                            Option.defaultValue ty_abbrev refined_ty
                        let tyctx = ctx.ty_ctx
                        if Typing.is_wf_type tyctx refined_ty
                           && Typing.is_subtype tyctx refined_ty ty_abbrev then
                            None,
                            Map.add entity.LogicalName refined_ty ctx.ty_map
                        else
                            Some
                                (TypeError
                                     (sprintf "Invalid refinement annotation %A"
                                          refined_attribute)), ctx.ty_map
                    else None, ctx.ty_map

                let ctx = { ctx with ty_map = ty_map }
                let errors, ctx = List.mapFold check_term ctx decls
                let errors = List.concat errors

                let errors =
                    match error_opt with
                    | Some e -> e :: errors
                    | None -> errors
                errors, ctx
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(member_or_func,
                                                                          arguments,
                                                                          e) ->
                let arguments =
                    List.concat arguments
                    |> List.filter (fun argument -> argument.IsValue)
                let argument_names =
                    List.map
                        (fun (arg : FSharpMemberOrFunctionOrValue) ->
                        arg.FullName) arguments
                let refined_attribute =
                    find_refined_attribute member_or_func.Attributes
                let refined_ty = Option.map attribute_to_ty refined_attribute

                let ty =
                    match refined_ty with
                    | Some _ as ty -> ty
                    | None ->
                        Option.map
                            (fun ty -> extract_type ctx ty argument_names)
                            member_or_func.FullTypeSafe

                let e = extract_expr ctx ty e
                let e =
                    List.foldBack (fun arg e -> Abs(arg, e)) argument_names e
                let ty_ctx = ctx.ty_ctx

                let errors, ty_ctx =
                    match ty with
                    | Some ty ->
                        if Typing.check_type ty_ctx e ty then
                            [],
                            Typing.env_add_var member_or_func.FullName ty ty_ctx
                        else
                            [ TypeError(sprintf "Incorrect Type %A for %A" ty e) ],
                            ty_ctx
                    | None ->
                        match Typing.infer_type ty_ctx e with
                        | Some ty ->
                            [],
                            Typing.env_add_var member_or_func.FullName ty ty_ctx
                        | None ->
                            [ TypeError(sprintf "Cannot infer type for %A" e) ],
                            ty_ctx
                errors, { ctx with ty_ctx = ty_ctx }
            | FSharpImplementationFileDeclaration.InitAction _ -> [], ctx

        let check_term' ctx decl =
            try
                check_term ctx decl
            with UnExtractable error -> [ error ], ctx

        let errors, _ctx = List.mapFold check_term' default_ctx decl
        List.concat errors
