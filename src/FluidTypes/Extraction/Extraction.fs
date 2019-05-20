namespace FluidTypes.Extraction

module Extraction =
    open FSharp.Compiler.AbstractIL.Internal.Library
    open FSharp.Compiler.SourceCodeServices
    open FluidTypes.Refinements
    open FluidTypes.Annotations.AnnotationParser
    open FluidTypes.Errors

    exception UnExtractable of Error

    type TyMap = Map<string, Ty>

    type ExtractionCtx =
        { ty_ctx : TyCtx
          ty_map : TyMap }

    let unresolved_tys : Set<string> ref = ref Set.empty

    let default_ty_map : TyMap =
        Map.ofList [ "int", mk_basetype TInt
                     "bool", mk_basetype TBool
                     "unit", ProductType [] ]

    let default_ctx : ExtractionCtx =
        { ty_ctx = Typing.empty_ctx
          ty_map = default_ty_map }

    let counter = ref 0

    let fresh_name() =
        let name = sprintf "_%d" !counter
        counter := !counter + 1
        name

    let resolve_unknown_ty (name: string) (typedef : Ty) (ctx: ExtractionCtx) : ExtractionCtx =
        if Set.contains name !unresolved_tys
        then printfn "Resolved Type: %s" name
        { ty_ctx = Typing.env_resolve_unknown_ty name typedef ctx.ty_ctx
          ty_map = Map.map (fun _ -> Substitution.resolve_unknown_ty_in_ty name typedef) ctx.ty_map
        }

    let is_refined_attribute (attribute : FSharpAttribute) =
        let ty = attribute.AttributeType
        ty.FullName = "FluidTypes.Annotations.RefinedAttribute"

    let find_refined_attribute (attributes : FSharpAttribute seq) =
        Seq.tryHead (Seq.filter is_refined_attribute attributes)

    let rec find_unknowns ty : Set<string> =
        match ty with
        | BaseType _ -> Set.empty
        | FuncType (_, t1, t2) -> Set.union (find_unknowns t1) (find_unknowns t2)
        | UnknownType t -> Set.singleton t
        | ProductType tys -> Set.unionMany (List.map find_unknowns tys)
        | RecordType _ -> Set.empty

    let resolve_ty (ctx: ExtractionCtx) (ty: Ty) : Ty =
        let unknowns = find_unknowns ty
        let try_resolve ty unknown =
            match Map.tryFind unknown ctx.ty_map with
            | Some def -> Substitution.resolve_unknown_ty_in_ty unknown def ty
            | None -> ty
        Set.fold try_resolve ty unknowns

    let attribute_to_ty (ctx: ExtractionCtx) (attribute : FSharpAttribute) =
        let refinement = attribute.ConstructorArguments.[0] |> snd :?> string
        let ty = parse_ty refinement
        resolve_ty ctx ty

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
        | _ when ty.IsTupleType ->
            let args = ty.GenericArguments |> List.ofSeq
            ProductType(List.map (fun ty -> extract_type ctx ty []) args)
        | _ when ty.HasTypeDefinition ->
            let ty_map = ctx.ty_map
            let name = Option.attempt (fun () -> ty.TypeDefinition.FullName)
            let name = Option.defaultValue (ty.TypeDefinition.DisplayName) name
            match Map.tryFind name ty_map with
            | Some ty -> ty
            | None ->
                let typeName = name
                printfn "Unknown Type: %s" typeName
                unresolved_tys := Set.add typeName !unresolved_tys
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
            | null -> Tuple [] (* Unit *)
            | otherwise ->
                let const_value = const_value_obj.ToString()
                printfn "Unknown const %s" const_value
                UnknownTerm(const_value, extract_type ctx const_type [])
        | BasicPatterns.Value(value_to_get) -> Var value_to_get.FullName
        | BasicPatterns.FSharpFieldGet(Some(expr), _ty, field) ->
            let expr = extract_expr ctx None expr
            FieldGet(expr, field.Name)
        | BasicPatterns.NewRecord(ty_, fields) ->
            let ty_ = extract_type ctx ty_ []
            let record_name =
                match ty_ with
                | RecordType r -> r
                | _ -> failwithf "Internal Error: %A is not a record type" ty_
            let exprs = List.map (extract_expr ctx None) fields
            NewRecord(exprs, record_name)
        | BasicPatterns.NewTuple(ty_, terms) ->
            let terms = List.map (extract_expr ctx None) terms
            Tuple(terms)
        | otherwise ->
            let e = e.ToString()
            printfn "Unknown expression %s" e
            UnknownTerm(e, ty)

    let add_typedef (ctx : ExtractionCtx) (entity : FSharpEntity) =
        let ty_abbrev =
            extract_type ctx entity.AbbreviatedType []
        let refined_attribute =
            find_refined_attribute entity.Attributes
        let refined_ty =
            Option.map (attribute_to_ty ctx) refined_attribute
        let refined_ty =
            Option.defaultValue ty_abbrev refined_ty
        let tyctx = ctx.ty_ctx
        let is_valid_typedef =
            Typing.is_wf_type tyctx refined_ty
                && Typing.is_subtype tyctx refined_ty ty_abbrev
        let ctx =
            if is_valid_typedef
            then
                let ctx = { ctx with ty_map = Map.add entity.LogicalName refined_ty ctx.ty_map }
                resolve_unknown_ty entity.LogicalName refined_ty ctx
            else ctx
        ctx

    let extract_field (ctx : ExtractionCtx, def : RecordDef) (field : FSharpField) : ExtractionCtx * RecordDef =
        let name = field.Name
        let refined_attribute = find_refined_attribute field.PropertyAttributes
        let refined_ty = Option.map (attribute_to_ty ctx) refined_attribute
        let raw_ty = extract_type ctx field.FieldType []
        let refined_ty = Option.defaultValue raw_ty refined_ty
        let tyctx = ctx.ty_ctx
        if Typing.is_wf_type tyctx refined_ty then
            if Typing.is_subtype tyctx refined_ty raw_ty then
                let tyctx = Typing.env_add_var name refined_ty tyctx
                { ctx with ty_ctx = tyctx }, ((name, refined_ty) :: def)
            else
                failwithf "Invalid refinement type %A for %s due to subtyping" refined_ty name (* TODO *)
        else
            failwithf "Invalid refinement type %A for %s due to wf" refined_ty name (* TODO *)


    let add_record (ctx : ExtractionCtx) (entity: FSharpEntity) =
        let name = entity.FullName
        let fields = List.ofSeq entity.FSharpFields
        let old_ctx = ctx
        (* Clear var ctx, fill with fields in records *)
        let ctx = { ctx with ty_ctx = { ctx.ty_ctx with varCtx = Map.empty } }
        let ctx, def = List.fold extract_field (ctx, []) fields
        let recordDef = List.rev def
        let ty_ctx = Typing.env_add_record name recordDef old_ctx.ty_ctx
        let ctx = { old_ctx with ty_ctx = ty_ctx ; ty_map = Map.add name (RecordType name) old_ctx.ty_map }
        resolve_unknown_ty name (RecordType name) ctx

    let rec check_decl (ctx : ExtractionCtx)
            (decl : FSharpImplementationFileDeclaration) : ExtractionCtx =
        match decl with
        | FSharpImplementationFileDeclaration.Entity(entity, decls) ->
            let ctx =
                if entity.IsFSharpAbbreviation then add_typedef ctx entity
                else if entity.IsFSharpRecord then add_record ctx entity
                else ctx
            let ctx = List.fold check_decl ctx decls
            ctx
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(member_or_func,
                                                                      _,
                                                                      _) when member_or_func.IsCompilerGenerated ->
            (* Skip all the generated declarations *)
            ctx
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
            let refined_ty = Option.map (attribute_to_ty ctx) refined_attribute

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

            let ty_ctx =
                match ty with
                | Some ty ->
                    if Typing.check_type ty_ctx e ty then
                        Typing.env_add_var member_or_func.FullName ty ty_ctx
                    else ty_ctx
                | None ->
                    match Typing.infer_type ty_ctx e with
                    | Some ty ->
                        Typing.env_add_var member_or_func.FullName ty ty_ctx
                    | None -> ty_ctx
            { ctx with ty_ctx = ty_ctx }
        | FSharpImplementationFileDeclaration.InitAction _ -> ctx

    let check_file ctx (file : FSharpImplementationFileContents) =
        let decl = file.Declarations
        printfn "File %s" file.FileName

        let check_decl' ctx decl =
            try
                check_decl ctx decl
            with UnExtractable error -> ctx
        List.fold check_decl' ctx decl

    let check_impl_files (files: FSharpImplementationFileContents list) =
        let ctx = default_ctx
        let _ = List.fold check_file ctx files
        ()
