namespace RecipeTests
open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open Evaluator
open AST
open Combinator

[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member this.ingredient () =
        let input = "ing[lettuce, corn, beef, chips]"
        let expected = [[Ingredient("lettuce") ; Ingredient("corn") ; Ingredient("beef") ; Ingredient("chips")]]
        let result = parse ( input)
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

            
    [<TestMethod>]
        member this.evalIngredient () =
            let input = Title("cake")
            let expected = @"\title{\Huge\fontfamily{lmdh}\selectfont" + "\n" + "cake" + "\n}" + "\n" + @"\date{}" + "\n" + @"\maketitle" + "\n" + "\n"
            let result2 = evalTitle (input)
            Assert.AreEqual(expected, result2)
            

