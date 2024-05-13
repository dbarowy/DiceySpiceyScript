module Parser

open Combinator
open AST

let padl p = pright (pstr ", ") p

let singularIns: Parser<Expr> = pmany0 (psat (fun c -> c <> ',')) |>> (fun a ->  Instruction(stringify a)) <!> "instruction"

let insideIns = 
    pseq (pmany1 (pleft singularIns (pstr ", " <|> pstr ","))) singularIns (fun (is, i) -> is @ [i]) <!> "instructions"

let instruction = 
    pleft
        (pbetween 
            (pstr "Ins [") 
                insideIns 
            (pchar ']'))
        pws0

let singularIng: Parser<Expr> = pmany0 (psat (fun c -> c <> ',')) |>> (fun a ->  Ingredient(stringify a)) <!> "ingredient"

let insideIng = 
    pseq (pmany1 (pleft singularIng (pstr ", " <|> pstr ","))) singularIng (fun (is, i) -> is @ [i]) <!> "ingredients"

let ingredient = 
    pleft
        (pbetween 
            (pstr "Ing [") 
                insideIng 
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
