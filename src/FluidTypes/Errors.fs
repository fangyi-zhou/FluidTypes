namespace FluidTypes

module Errors =
    type TypeError =
        | VarNotFound of string
        | NotAFunction of string * string
        | NotInferrable of string
        | NotClosedType of string * string list
        | NotASubType of string * string * string
        | FieldNotFound of string * string
        | NotARecord of string

    (* TODO *)
    type ExtractionError = string

    type Error =
        | ExtractionError of ExtractionError
        | TypeError of TypeError

    let show_type_error (error : TypeError) =
        match error with
        | VarNotFound var -> sprintf "Cannot find variable %s in context" var
        | NotAFunction(term, ty) ->
            sprintf "Type for %s should be a function, but got %s" term ty
        | NotInferrable term -> sprintf "Cannot infer type for %s" term
        | NotClosedType(ty, vars) ->
            let vars = String.concat ", " vars
            sprintf "Type %s is not closed because it has free variables %s" ty
                vars
        | NotASubType(term, ty_1, ty_2) ->
            sprintf "%s is not a subtype of %s for term %s" ty_1 ty_2 term
        | FieldNotFound(record_ty, field_name) ->
            sprintf "%s is not a field name for record %s" field_name record_ty
        | NotARecord(term) ->
            sprintf "%s is not a record, cannot use field get." term

    let show_extraction_error (error : ExtractionError) = error

    let show_error (error : Error) =
        match error with
        | ExtractionError e -> show_extraction_error e
        | TypeError e -> show_type_error e

    let all_errors : Error list ref = ref []
    let has_errors () = not (List.isEmpty !all_errors)

    let report_errors() : int =
        if not (has_errors ()) then 0
        else
            (let errors = List.map show_error !all_errors
             List.iter (printfn "%s") errors
             1)

    let add_error e = all_errors := e :: !all_errors
    let add_type_error e = add_error (TypeError e)
    let err_var_not_found var = add_type_error (VarNotFound var)
    let err_not_a_function term ty = add_type_error (NotAFunction(term, ty))
    let err_not_inferrable term = add_type_error (NotInferrable term)
    let err_not_closed_type ty vars = add_type_error (NotClosedType(ty, vars))
    let err_not_a_subtype term ty_1 ty_2 =
        add_type_error (NotASubType(term, ty_1, ty_2))
    let err_field_not_found record field =
        add_type_error (FieldNotFound (record, field))
    let err_not_a_record term = add_type_error (NotARecord term)
