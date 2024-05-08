module Evaluator

open AST
open System


let evalExpr (section) : string =
    match section with
    | Title(a) -> @"\section{" + a + "}\n" 
    | Ingredient(b) -> @"{\Large Ingredients:}" + @"\\" + b + @"\\" + "\n"
    | Instruction(c) -> @"{\Large Instructions:}" + @"\\" + c + @"\\" + "\n"

let rec evalRecipe exprList : string =
    match exprList with
    | [] -> ""
    | l::ls -> (evalExpr l) + (evalRecipe ls)

let rec sortList unsortedRecipe sortedRecipe sortedRecipe = 
     match unsortedRecipe with
    | [] -> sortedRecipe
    | l::ls -> 
        match l with 
        | Title(a) -> sortedRecipe. 
        | Ingredient(b) -> 
        | Instruction(c) -> 

let eval unsortedRecipe : string =
    @"\documentclass{article}" + "\n" + @"\usepackage{graphicx}" + "\n" + @"\begin{document}" + "\n" + 
    (evalRecipe (sortList unsortedRecipe sortedRecipe))
    + @"\end{document}"


