module Evaluator

open AST
open System

let evalTitle (title: Expr) : string =
    match title with
    | Title(a) -> @"\section{" + a + "}\n" 
    | _ -> 
        printfn "Title is somehow not a string object"
        exit(0)

let rec evalIngredient (instructions : Expr list) : string =
    match instructions with 
    | [] -> ""
    | l::ls -> 
        match l with
        | Ingredient(a) -> @"{\Large Ingredients:}" + @"\\" + a + @"\\" + "\n" + (evalIngredient ls)
        | _ -> 
            printfn "This ingredient is somehow not a string object"
            exit(0)

let rec evalInstruction (instructions : Expr list) : string =
    match instructions with 
    | [] -> ""
    | l::ls -> 
        match l with
        | Instruction(a) -> @"{\Large Instructions:}" + @"\\" + a + @"\\" + "\n" + (evalInstruction ls)
        | _ -> 
            printfn "This instruction is somehow not a string object"
            exit(0)

let evalRecipe (r : Recipe) : string =
    (evalTitle r.Title) + (evalIngredient r.Ingredients) + (evalInstruction r.Instructions)

let sortHelper (ingredientList : Expr list) (sortedRecipe : Recipe) = 
    match ingredientList with
        | [] -> sortedRecipe
        | l::ls ->  
                
    
let rec sortList (unsortedRecipe : Expr list list) (sortedRecipe : Recipe) = 
    match unsortedRecipe with
    | [] -> sortedRecipe
    | l::ls -> 
        match l with 
        | Title list ->
            match l with 
            | [] -> sortedRecipe
            | m::ms ->   
                match m with
                | Title -> 
                    let r : Recipe = 
                        {
                        Title = m;
                        Ingredients = sortedRecipe.Ingredients;
                        Instructions = sortedRecipe.Instructions;
                        }
                    sortList ls r 
                | _ -> 
                    printfn "%A" "There's an issue with the Title evaluator" 
                    exit 0
        | Ingredient list ->
            let r= 
                {
                    Title = sortedRecipe.Title;
                    Ingredients = sortedRecipe.Ingredients @ l;
                    Instructions = sortedRecipe.Instructions;
                }
            sortList ls r
        | Instruction list -> 
            let r= 
                {
                    Title = sortedRecipe.Title;
                    Ingredients = sortedRecipe.Ingredients;
                    Instructions = sortedRecipe.Instructions @ l;
                }
            sortList ls r
        | _ -> 
            printfn "%A" "The recipe must be empty, please put something in your recipe" 
            exit 0

let eval unsortedRecipe : string =
    @"\documentclass{article}" + "\n" + @"\usepackage{graphicx}" + "\n" + @"\begin{document}" + "\n" + 
    (evalRecipe (sortList unsortedRecipe sortedRecipe))
    + @"\end{document}"


