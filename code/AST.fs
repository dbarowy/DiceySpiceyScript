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

