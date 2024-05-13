module Parser

open Combinator
open AST

let padl p = pright (pstr ", ") p

let singular: Parser<Expr> = pmany0 (psat (fun c -> c <> ',')) |>> (fun a ->  Instruction(stringify a)) <!> "instruction"

let inside = pseq (pmany1 (pleft singular (pstr ", " <|> pstr ","))) singular //|>> (fun (is, i) -> is @ [i]) <!> "instructions"

let instruction = 
    pleft
        (pbetween 
            (pstr "Ins [") 
                inside 
            (pchar ']'))
        pws0

let ingredient = 
    pleft
        (pbetween 
            (pstr "Ing [") 
                inside 
            (pchar ']'))
        pws0
let title = 
    pleft
        (pbetween 
            (pstr "Tit [") 
            (pmany1 pletter |>> (fun e -> [Title (stringify e)])) 
            (pchar ']'))
        pws0

let expr = pmany1 (instruction <|> ingredient <|> title)

let parse input =
    // parser
    let grammar = pleft expr peof
    
    // parse input
    match grammar (prepare input) with
    | Success(res,_) -> Some res
    | Failure _ -> None
