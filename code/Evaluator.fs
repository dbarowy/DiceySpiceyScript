module Evaluator

open AST
open System

let evalTitle (title: Expr) : string =
    match title with
    | Title(a) ->  
        ( @"\title{\Huge\fontfamily{lmdh}\selectfont" + "\n" + 
        a 
        + "\n}" + "\n" + @"\date{}" + "\n" + @"\maketitle" + "\n" + "\n")
    | _ -> 
        printfn "Title is somehow not a string object"
        exit 1

let rec evalIngredient (ingredients : Expr list) : string =
    match ingredients with 
    | [] -> ""
    | l::ls -> 
        match l with
        | Ingredient(a) -> @"\item " + a  + "\n" + (evalIngredient ls)
        | _ -> 
            printfn "This ingredient is somehow not a string object"
            exit 1

let rec evalInstruction (instructions : Expr list) : string =
    match instructions with 
    | [] -> ""
    | l::ls -> 
        match l with
        | Instruction(a) -> @"\item " + a + "\n" + (evalInstruction ls)
        | _ -> 
            printfn "This instruction is somehow not a string object"
            exit 1

let evalRecipe (r : Recipe) : string =
    (evalTitle r.Title) + 
    @"\begin{mdframed}" + "\n" + @"{\Large\fontfamily{lmdh}\selectfont" + "\n" + "  " + "Ingredients:"
    + @"}" + "\n\n" + "" + @"\begin{itemize}" + "\n" + 
    (evalIngredient r.Ingredients) + 
    @"\end{itemize}" + "\n" + @"\end{mdframed}" + "\n" + @"\begin{mdframed}" + "\n"+ 
    @"{\Large\fontfamily{lmdh}\selectfont" + "\n" + "  " + "Instructions:"
    + @"}"+ "\n\n" + @"\begin{enumerate}" + "\n" + 
    (evalInstruction r.Instructions) + @"\end{enumerate}" + "\n" + @"\end{mdframed}" + "\n\n"  
                
let rec sortList (unsortedRecipe : Expr list list) (sortedRecipe : Recipe) = 
    match unsortedRecipe with
    | [] -> sortedRecipe
    | subList::bigList -> 
        match subList with 
        | [] -> sortedRecipe
        | firstElement::other -> 
            match firstElement with 
            | Title(a) ->
                let r : Recipe = 
                    {
                        Title = firstElement;
                        Ingredients = sortedRecipe.Ingredients;
                        Instructions = sortedRecipe.Instructions;
                    }
                sortList bigList r 
            | Ingredient(a) ->
                let r= 
                    {
                        Title = sortedRecipe.Title;
                        Ingredients = sortedRecipe.Ingredients @ subList;
                        Instructions = sortedRecipe.Instructions;
                    }
                sortList bigList r
            | Instruction(a) -> 
                let r= 
                    {
                        Title = sortedRecipe.Title;
                        Ingredients = sortedRecipe.Ingredients;
                        Instructions = sortedRecipe.Instructions @ subList;
                    }
                sortList bigList r
            | _ -> 
                printfn "%A" "You have something other than a title, instruction, or ingredient. 
                            Please fix your text to only have Title[], Ing[], or Ins[]." 
                exit 1

let eval unsortedRecipe : string =
    @"\documentclass{article}" + "\n" + @"\usepackage[T1]{fontenc}" + "\n" + @"\usepackage{tgbonum}" + "\n"
    + @"\usepackage{mdframed}" + "\n" + "\n" +
    @"\begin{document}" + "\n" + "\n" +
    (evalRecipe (sortList unsortedRecipe sortedRecipe))
    + @"\end{document}"


