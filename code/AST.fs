module AST

type Expr = 
| Instruction of string
| Ingredient of string
| Title of string