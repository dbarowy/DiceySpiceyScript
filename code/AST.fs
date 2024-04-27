module AST

type Expr = 
| String of string //title, ingredient, instruction
| Num of int //quantity of measurement for ingredient
| Unit of int //unit of measurement for ingredient
| Ingred of string //what is ingredient
| StepNum of int //step number
| StepContent of string 
| Instruction of Expr * Expr * Expr //String StepNum StepContent
| Ingredient of Expr* Expr * Expr * Expr //String Num Unit Ingred
