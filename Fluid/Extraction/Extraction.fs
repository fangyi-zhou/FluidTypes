namespace FluidTypes.Extraction

module Extraction =
    open FSharp.Compiler.SourceCodeServices
    open FluidTypes.Refinements

    exception UnExtractable

    let rec extract_expr (e: FSharpExpr) : Term =
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
            | _ -> raise UnExtractable
        | BasicPatterns.Value (value_to_get) ->
            Var value_to_get.FullName
        | _ -> raise UnExtractable


    let countDecl (fileContents : FSharpImplementationFileContents) =
        let decl = fileContents.Declarations in
        let rec checkDecl (decl : FSharpImplementationFileDeclaration) : int =
            match decl with
            | FSharpImplementationFileDeclaration.Entity (_entity, decls) ->
                List.sumBy checkDecl decls
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (member_or_func, arguments, e) ->
                eprintf "Extraction: %A (%A) = %A\n" member_or_func arguments (extract_expr e);
                1
            | FSharpImplementationFileDeclaration.InitAction _ ->
                1
        in
        let declCounts = List.map checkDecl decl in
        List.sum declCounts
