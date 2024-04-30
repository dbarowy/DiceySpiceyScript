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

let eval recipe : string =
    @"\documentclass{article}" + "\n" + @"\usepackage{graphicx}" + "\n" + @"\begin{document}" + "\n" + 
    (evalRecipe recipe)
    + @"\end{document}"


