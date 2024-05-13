module Parser

open Combinator
open AST

let padl p = pright (pstr ", ") p

let singularIns: Parser<Expr> = pmany0 (psat (fun c -> c <> ','&& c <> ']')) |>> (fun a ->  Instruction(stringify a)) <!> "instruction"

let insideIns = 
    pseq (pmany0 (pleft singularIns (pstr ", "))) singularIns (fun (is, i) -> is @ [i]) <!> "instructions"
//<|> pstr ","
let instruction = 
    pleft
        (pbetween (pstr "ins[") insideIns (pchar ']'))
        pws0

let singularIng: Parser<Expr> = pmany0 (psat (fun c -> c <> ',' && c <> ']')) |>> (fun a ->  Ingredient(stringify a)) <!> "ingredient"

let insideIng = 
    pseq (pmany0 (pleft singularIng (pstr ", "))) singularIng (fun (is, i) -> is @ [i]) <!> "ingredients"
// <|> pstr ","
let ingredient = 
    pleft
        (pbetween 
            (pstr "ing[") 
                insideIng 
            (pchar ']'))
        pws0
let title = 
    pleft
        (pbetween 
            (pstr "tit[") 
            (pmany0 (psat (fun c -> c <> ']')) |>> (fun e -> [Title (stringify e)]))
            (pchar ']'))
        pws0

let expr = pmany1 (instruction <|> ingredient <|> title)

let parse input =
    // parser
    let grammar = pleft expr peof
    
    // parse input
    match grammar (debug input) with
    | Success(res,_) -> Some res
    | Failure _ -> None
