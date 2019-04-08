namespace FluidTypes

module Errors =
    (* TODO *)
    type ExtractionError = string
    type TypeError = string

    type Error =
        | ExtractionError of ExtractionError
        | TypeError of TypeError
