module Parser

open Combinator
open AST

let instruction : Parser<Expr> = 
    pleft
        (pbetween 
            (pstr "Instructions(") 
            (pmany1 (pletter <|> pchar ' ' <|> pchar ',' <|> pdigit) |>> (fun e -> Instruction (stringify e))) 
            (pchar ')'))
        pws0

let ingredient : Parser<Expr> = 
    pleft
        (pbetween 
            (pstr "Ingredients(") 
            (pmany1 (pletter <|> pchar ' ' <|> pchar ',' <|> pdigit) |>> (fun e -> Ingredient (stringify e))) 
            (pchar ')'))
        pws0
let title : Parser<Expr> = 
    pleft
        (pbetween 
            (pstr "Title(") 
            (pmany1 pletter |>> (fun e -> Title (stringify e))) 
            (pchar ')'))
        pws0

let expr = pmany1 (instruction <|> ingredient <|> title)

let parse input =
    // parser
    let grammar = pleft expr peof
    
    // parse input
    match grammar (prepare input) with
    | Success(res,_) -> Some res
    | Failure _ -> None
