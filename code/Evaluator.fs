module Evaluator

open AST
open System


let evalExpr (section) : string =
    match section with
    | Title(a) -> @"\section{" + a + "}\n" 
    | Ingredient(b) -> @"{\Large Ingredients:}" + @"\\" + b + @"\\" + "\n"
    | Instruction(c) -> @"{\Large Instructions:}" + @"\\" + c + @"\\" + "\n"

let rec evalRecipe Recipe : string =
    match exprList with
    | [] -> ""
    | l::ls -> (evalExpr l) + (evalRecipe ls)

let rec sortList (unsortedRecipe : Expr list) (sortedRecipe : Recipe) = 
    match unsortedRecipe with
    | [] -> sortedRecipe
    | l::ls -> 
        match l with 
        | Title(a) -> 
            let r = 
                {
                    Title = l;
                    Ingredients = sortedRecipe.Ingredients;
                    Instructions = sortedRecipe.Instructions;
                }
            sortList ls r 
        | Ingredient(b) -> 
             let r= 
                {
                    Title = l;
                    Ingredients = sortedRecipe.Ingredients @ [l];
                    Instructions = sortedRecipe.Instructions;
                }
             sortList ls r
        | Instruction(c) -> 
             let r= 
                {
                    Title = l;
                    Ingredients = sortedRecipe.Ingredients;
                    Instructions = sortedRecipe.Instructions @ [l];
                }
             sortList ls r

let eval unsortedRecipe : string =
    @"\documentclass{article}" + "\n" + @"\usepackage{graphicx}" + "\n" + @"\begin{document}" + "\n" + 
    (evalRecipe (sortList unsortedRecipe sortedRecipe))
    + @"\end{document}"


