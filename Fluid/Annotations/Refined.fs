namespace FluidTypes.Annotations

module Refined =
    open FluidTypes.Refinements
    type RefinedIntAttribute(refinement : string) =
        inherit System.Attribute()
        member this.Refinement : string = refinement

        (* TODO *)
        member this.RefinedType : Ty = mk_basetype TInt