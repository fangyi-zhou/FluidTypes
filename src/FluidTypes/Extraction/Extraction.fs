namespace FluidTypes.Extraction

module Extraction =
    open FSharp.Compiler.SourceCodeServices
    open FluidTypes.Refinements
    open FluidTypes.Annotations.AnnotationParser

    type TyMap = Map<string, Ty>

    type ExtractionCtx =
        { ty_ctx : TyCtx
          ty_map : TyMap
          enum_values : Map<string, Map<string, int>> }

    let unresolved_tys : Set<string> ref = ref Set.empty

    let default_ty_map : TyMap =
        Map.ofList [ "int", mk_basetype TInt
                     "bool", mk_basetype TBool
                     "unit", ProductType [] ]

    let default_ctx : ExtractionCtx =
        { ty_ctx = Typing.empty_ctx
          ty_map = default_ty_map
          enum_values = Map.empty }

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
          enum_values = ctx.enum_values
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
        | UnionType _ -> Set.empty

    let ensure_union ty =
        match ty with
        | UnionType u -> u
        | _ -> failwithf "Internal Error: %A is not a union type" ty

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
            let name = try Some (ty.TypeDefinition.FullName) with _ -> None
            let name = Option.defaultValue (ty.TypeDefinition.DisplayName) name
            let name = remove_namespace name
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

    let branches_stack = ref []

    let rec extract_expr (ctx : ExtractionCtx) (e : FSharpExpr) : Term =
        let ty = extract_type ctx e.Type []
        match e with
        | BasicPatterns.Application(func_expr, type_args, arg_exprs) ->
            let func = extract_expr ctx func_expr
            let args = List.map (extract_expr ctx) arg_exprs
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
                | "Microsoft.FSharp.Core.Operators.failwith" -> Diverge
                | name -> Var name

            let obj_opt = Option.map (extract_expr ctx) obj_expr_opt
            let args = List.map (extract_expr ctx) arg_exprs

            match obj_opt with
            | Some _ ->
                let e = e.ToString()
                printfn "Unknown expression %s" e
                UnknownTerm(e, ty)
            | None ->
                match func with
                | Diverge -> Diverge
                | _ -> List.fold (fun f a -> App(f, a)) func args
        | BasicPatterns.IfThenElse(guard_expr, then_expr, else_expr) ->
            let cond = extract_expr ctx guard_expr
            let then_ = extract_expr ctx then_expr
            let else_ = extract_expr ctx else_expr
            IfThenElse(cond, then_, else_)
        | BasicPatterns.Lambda(lambda_var, body_expr) ->
            let body = extract_expr ctx body_expr
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
            let expr = extract_expr ctx expr
            FieldGet(expr, field.Name)
        | BasicPatterns.NewRecord(ty_, fields) ->
            let ty_ = extract_type ctx ty_ []
            let record_name =
                match ty_ with
                | RecordType r -> r
                | _ -> failwithf "Internal Error: %A is not a record type" ty_
            let exprs = List.map (extract_expr ctx) fields
            NewRecord(exprs, record_name)
        | BasicPatterns.NewTuple(ty_, terms) ->
            let terms = List.map (extract_expr ctx) terms
            Tuple(terms)
        | BasicPatterns.Let((var, binding_expr), body_expr) ->
            let binding_expr = extract_expr ctx binding_expr
            let varName = var.FullName
            let ty_binding = Option.map (fun ty -> extract_type ctx ty []) var.FullTypeSafe
            let binding_expr =
                match ty_binding with
                | Some ty_binding when not (is_inferrable binding_expr) -> Anno (binding_expr, ty_binding)
                | _ -> binding_expr
            let body_expr = extract_expr ctx body_expr
            Let(varName, binding_expr, body_expr)
        | BasicPatterns.DecisionTree(e, branches) ->
            let branches = List.map (snd >> extract_expr ctx) branches
            branches_stack := branches :: !branches_stack
            let e = extract_expr ctx e
            branches_stack := List.tail (!branches_stack)
            e
        | BasicPatterns.DecisionTreeSuccess(idx, _) ->
            let branches = List.head (!branches_stack)
            List.item idx branches
        | BasicPatterns.UnionCaseTest(e, ty_, union_case) ->
            let e = extract_expr ctx e
            let ty_ = extract_type ctx ty_ []
            let union_name = ensure_union ty_
            let case_name = union_case.Name
            App (Var (get_union_test_name union_name case_name), e)
        | BasicPatterns.UnionCaseGet(e, ty_, union_case, _) ->
            let e = extract_expr ctx e
            let ty_ = extract_type ctx ty_ []
            let union_name = ensure_union ty_
            let case_name = union_case.Name
            App (Var (get_union_getter_name union_name case_name), e)
        | BasicPatterns.NewUnionCase(ty_, union_case, es) ->
            let es = List.map (extract_expr ctx) es
            let e =
                match es with
                | [e] -> e
                | es -> Tuple es
            let ty_ = extract_type ctx ty_ []
            let union_name = ensure_union ty_
            let case_name = union_case.Name
            App (Var (get_union_constructor_name union_name case_name), e)
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
                && Typing.eq_simple_ty refined_ty ty_abbrev
        let ctx =
            if is_valid_typedef
            then
                let ctx = { ctx with ty_map = Map.add (remove_namespace entity.LogicalName) refined_ty ctx.ty_map }
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
            if Typing.eq_simple_ty refined_ty raw_ty then
                let tyctx = Typing.env_add_var name refined_ty tyctx
                { ctx with ty_ctx = tyctx }, ((name, refined_ty) :: def)
            else
                printfn "%A" raw_ty
                printfn "%A" refined_ty
                failwithf "Invalid refinement type %A for %s due to not compatible" refined_ty name (* TODO *)
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
        let name = remove_namespace name
        let ty_ctx = Typing.env_add_record name recordDef old_ctx.ty_ctx
        let ctx = { old_ctx with ty_ctx = ty_ctx ; ty_map = Map.add name (RecordType name) old_ctx.ty_map }
        resolve_unknown_ty name (RecordType name) ctx

    let extract_union_case (ctx : ExtractionCtx) (def : UnionDef) (unionCase : FSharpUnionCase) : UnionDef =
        (* FIXME: Refactor duplicated code c.f. records *)
        let name = unionCase.Name
        let raw_ty = List.ofSeq (Seq.map (fun (f: FSharpField) -> f.FieldType) unionCase.UnionCaseFields)
        let raw_ty =
            match raw_ty with
            | [ty] -> extract_type ctx ty []
            | _ -> ProductType (List.map (fun ty -> extract_type ctx ty []) raw_ty)
        let refined_attribute = find_refined_attribute unionCase.Attributes
        let refined_ty = Option.map (attribute_to_ty ctx) refined_attribute
        let refined_ty = Option.defaultValue raw_ty refined_ty
        let tyctx = ctx.ty_ctx
        if Typing.is_wf_type tyctx refined_ty then
            if Typing.eq_simple_ty refined_ty raw_ty then
                (name, refined_ty) :: def
            else
                printfn "%A" raw_ty
                printfn "%A" refined_ty
                failwithf "Invalid refinement type %A for %s due to not compatible" refined_ty name (* TODO *)
        else
            failwithf "Invalid refinement type %A for %s due to wf" refined_ty name (* TODO *)

    let add_union (ctx: ExtractionCtx) (entity: FSharpEntity) =
        let name = entity.FullName
        let union_cases = List.ofSeq entity.UnionCases
        let old_ctx = ctx
        (* Clear var ctx *)
        let ctx = { ctx with ty_ctx = { ctx.ty_ctx with varCtx = Map.empty } }
        let defs = List.fold (extract_union_case ctx) [] union_cases
        let unionDef = List.rev defs
        let name = remove_namespace name
        let ty_ctx = Typing.env_add_union name unionDef old_ctx.ty_ctx
        let ctx = { old_ctx with ty_ctx = ty_ctx ; ty_map = Map.add name (UnionType name) old_ctx.ty_map }
        resolve_unknown_ty name (UnionType name) ctx

    let add_enum (ctx: ExtractionCtx) (entity: FSharpEntity) =
        let name = remove_namespace entity.FullName
        let extract_value values (field : FSharpField) =
            if field.Name = "value__"
            then values (* Ignore Compiler Generated Field *)
            else
                let value = (Option.get field.LiteralValue) :?> int
                Map.add field.Name value values
        let values = Seq.fold extract_value Map.empty entity.FSharpFields
        { ctx
            with
                enum_values = Map.add name values ctx.enum_values
                (* Enums are integers *)
                ty_map = Map.add name (mk_basetype TInt) ctx.ty_map
        }

    let rec check_decl (ctx : ExtractionCtx)
            (decl : FSharpImplementationFileDeclaration) : ExtractionCtx =
        match decl with
        | FSharpImplementationFileDeclaration.Entity(entity, decls) ->
            let ctx =
                if entity.IsFSharpAbbreviation then add_typedef ctx entity
                else if entity.IsFSharpRecord then add_record ctx entity
                else if entity.IsFSharpUnion then add_union ctx entity
                else if entity.IsEnum then add_enum ctx entity
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

            let e = extract_expr ctx e
            let e =
                List.foldBack (fun arg e -> Abs(arg, e)) argument_names e
            let ty_ctx = ctx.ty_ctx

            let ty_ctx =
                match ty with
                | Some ty ->
                    let _ = Typing.check_type ty_ctx e ty
                    Typing.env_add_var member_or_func.FullName ty ty_ctx
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
        List.fold check_decl ctx decl

    let check_impl_files (files: FSharpImplementationFileContents list) =
        let ctx = default_ctx
        let _ = List.fold check_file ctx files
        ()
