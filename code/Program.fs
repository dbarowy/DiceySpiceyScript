open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main argv : int =
    (* Check for proper usage *)
    if argv.Length <> 1 && argv.Length <> 2 then
        printfn "Usage: dotnet run <file> \n file contains format: \n \tTitle(title of recipe)
\n \tIngredients(ingredient 1, ingredient 2, ingredient 3, ...)
\n \tInstructions(instruction 1, instruction2, instruction 3, ...)"
        exit 1

    (* read in the input file *)
    let file = argv.[0]
    let input = File.ReadAllText file

    (* does the user want parser debugging turned on? *)
    //let do_debug = if argv.Length = 2 then true else false

    (* try to parse what they gave us *)
    let ast_maybe = parse input

    (* try to evaluate what we parsed... or not *)
    match ast_maybe with
    | Some ast ->
        printf "%A" (eval ast)
        0
    | None ->
        printfn "Invalid program."
        1