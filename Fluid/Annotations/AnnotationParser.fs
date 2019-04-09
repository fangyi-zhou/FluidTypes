namespace FluidTypes.Annotations

module AnnotationParser =
    open FluidTypes.Refinements
    let parse_annotation (input: string) : Term =
        Const (BoolLiteral true)