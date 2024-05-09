module Parser

open Combinator
open AST

let ingredient2, ingredient = recparser()

let instruction2,instruction = recparser()
let comma item = pright (pchar ',') item
let instruction2 = 
    (pmany1 (pletter <|> pchar ' ' <|> pdigit ) |>> (fun e -> Instruction (stringify e))) 

let instruction =
    instruction2 <|> instruction2 comma instruction
let instructions = 
    pleft
        (pbetween 
            (pstr "Ins (") 
                instruction
            (pchar ')'))
        pws0

let ingredient2 = 
    pmany1 (pletter <|> pchar ' ' <|> pdigit) |>> (fun e -> Ingredient (stringify e))
let ingredient =
    ingredient2 <|> ingredient2 comma ingredient
let ingredients = 
    pleft
        (pbetween 
            (pstr "Ing (") 
            (ingredient) 
            (pchar ')'))
        pws0
let title = 
    pleft
        (pbetween 
            (pstr "Tit (") 
            (pmany1 pletter |>> (fun e -> Title (stringify e))) 
            (pchar ')'))
        pws0

let expr = pmany1 (instructions <|> ingredients <|> title)

let parse input =
    // parser
    let grammar = pleft expr peof
    
    // parse input
    match grammar (prepare input) with
    | Success(res,_) -> Some res
    | Failure _ -> None
