namespace FluidTypes.Annotations

module AnnotationParser =
    open FSharp.Text.Lexing
    open FluidTypes.Errors
    open FluidTypes.Refinements
    open FluidTypes.Annotations.Parser
    let parse_term (input: string) : Term =
        let lexbuf = LexBuffer<char>.FromString input in
        try
            Parser.expr Lexer.token lexbuf
        with
        | e -> failwith (sprintf "Cannot parse %s" input)
