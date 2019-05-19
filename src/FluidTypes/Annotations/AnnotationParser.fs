namespace FluidTypes.Annotations

module AnnotationParser =
    open FSharp.Text.Lexing
    open FluidTypes.Refinements

    let parse_term (input : string) : Term =
        let lexbuf = LexBuffer<char>.FromString input
        try
            Parser.expr Lexer.token lexbuf
        with e -> failwith (sprintf "Cannot parse %s" input)

    let parse_ty (input : string) : Ty =
        let lexbuf = LexBuffer<char>.FromString input
        try
            Parser.ty Lexer.token lexbuf
        with e -> failwith (sprintf "Cannot parse %s" input)
