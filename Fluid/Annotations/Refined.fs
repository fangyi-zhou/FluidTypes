namespace FluidTypes.Annotations

module Refined =
    open FluidTypes.Refinements
    open FluidTypes.Annotations.AnnotationParser
    type RefinedIntAttribute(refinement : string) =
        inherit System.Attribute()
        member this.Refinement : string = refinement
        member this.RefinedType : Ty =
            let term = parse_term refinement in
            BaseType (TInt, term)
