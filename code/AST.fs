module AST

type Expr = 
| Instruction of string
| Ingredient of string
| Title of string

type Recipe = 
    {
        Title: Expr;
        Ingredients: Expr list;
        Instructions: Expr list; 
    }

let STARTINGTITLE = Title "Unnamed Recipe"
let STARTINGLIST_ING = [];
let STARTINGLIST_INS = [];

let sortedRecipe : Recipe = 
    {
        Title = STARTINGTITLE; 
        Ingredients = STARTINGLIST_ING; 
        Instructions = STARTINGLIST_INS;
    }
