namespace FluidTypes

module Typing = 

    let type_const (c: Constant) : Ty =
        failwith "Unimplemented"

    let rec infer_type (ctx: TyCtx) (term: Term) : Ty option = 
        match term with
        | Const c -> Some (type_const c)
        | _ -> failwith "Unimplemented"
    
    and check_type (ctx: TyCtx) (term: Term) (ty: Ty) : bool =
        false

    and check_simple_type (ctx: TyCtx) (term: Term) (ty: Ty) : bool = 
        false
