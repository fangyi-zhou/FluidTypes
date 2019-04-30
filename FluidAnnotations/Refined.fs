namespace FluidTypes.Annotations

type RefinedAttribute(refinement : string) =
    inherit System.Attribute()
    member this.Refinement = refinement
