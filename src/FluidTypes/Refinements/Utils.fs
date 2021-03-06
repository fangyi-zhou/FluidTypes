namespace FluidTypes.Refinements

[<AutoOpen>]
module Utils =
    let mk_int i = Const(IntLiteral i)
    let mk_bool b = Const(BoolLiteral b)
    let mk_binop_app b x y = App(App(Const(Binop b), x), y)

    let mk_this_eq_term (base_ty : BaseTy) (t : Term) : Term =
        let eq =
            match base_ty with
            | TInt -> EqualInt
            | TBool -> EqualBool
        mk_binop_app eq (Var special_this) t

    let mk_basetype (base_ty : BaseTy) : Ty =
        BaseType(base_ty, Const(BoolLiteral true))
    let mk_not (term : Term) : Term = App(Const(Unop Not), term)

    let mk_binop_type (b : Binop) : Ty =
        let binop_term = App(App(Const(Binop b), (Var "x")), (Var "y"))

        let ty_arg, ty_result =
            match b with
            | Plus
            | Minus -> TInt, TInt
            | And
            | Or
            | EqualBool
            | NotEqualBool -> TBool, TBool
            | EqualInt
            | NotEqualInt
            | Greater
            | GreaterEqual
            | Less
            | LessEqual -> TInt, TBool

        let refinement_term = mk_this_eq_term ty_result binop_term
        FuncType
            ("x", mk_basetype ty_arg,
             (FuncType
                  ("y", mk_basetype ty_arg, BaseType(ty_result, refinement_term))))

    let mk_unop_type (u : Unop) : Ty =
        let unop_term = App(Const(Unop u), (Var "x"))

        let ty_arg, ty_result =
            match u with
            | Negate -> TInt, TInt
            | Not -> TBool, TBool

        let refinement_term = mk_this_eq_term ty_result unop_term
        FuncType("x", mk_basetype ty_arg, BaseType(ty_result, refinement_term))

    let mk_equal_int i = mk_binop_app EqualInt (Var special_this) (mk_int i)
    let mk_equal_bool b = mk_binop_app EqualBool (Var special_this) (mk_bool b)

    let read_basetype str =
        match str with
        | "int" -> TInt
        | "bool" -> TBool
        | _ -> failwith "Unknown type"

    let remove_namespace (ty_name: string) =
        let last_dot = ty_name.LastIndexOf(".")
        if last_dot < 0 then
            ty_name
        else ty_name.Substring(last_dot + 1)

    let rec is_inferrable (term: Term) : bool =
        match term with
        | Var _
        | Const _
        | Anno _
        | Coerce _
        | UnknownTerm _ ->
            true
        | App (t, _)
        | FieldGet (t, _) ->
            is_inferrable t
        | _ ->
            false

    let get_union_test_name union_name case_name =
        sprintf "$%s_is%s" union_name case_name

    let get_union_getter_name union_name case_name =
        sprintf "$%s_get%s" union_name case_name

    let get_union_constructor_name union_name case_name =
        sprintf "$%s_ctor%s" union_name case_name
