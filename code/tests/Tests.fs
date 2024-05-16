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
    member this.instruction () =
        let input = "ins[Grease pan, mix in all ingredients for batter, pour batter into round cake pan, bake for 25 min]"
        let expected = [[Instruction("Grease pan") ; Instruction("mix in all ingredients for batter") ; Instruction("pour batter into round cake pan") ; Instruction("bake for 25 min")]]
        let result = parse ( input)
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.title () =
        let input = "tit[Apple Pie]"
        let expected = [[Title("Apple Pie")]]
        let result = parse ( input)
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

            
    [<TestMethod>]
        member this.evalTitle () =
            let input = Title("cake")
            let expected = @"\title{\Huge\fontfamily{lmdh}\selectfont" + "\n" + "cake" + "\n}" + "\n" + @"\date{}" + "\n" + @"\maketitle" + "\n" + "\n"
            let result2 = evalTitle (input)
            Assert.AreEqual(expected, result2)

    [<TestMethod>]
        member this.evalInstruction () =
            let input = [Instruction("Grease Pan")] @ [Instruction("Dance")]
            let expected =  @"\item " + "Grease Pan" + "\n" + @"\item " + "Dance" + "\n"
            let result2 = evalInstruction (input)
            Assert.AreEqual(expected, result2)
            

    [<TestMethod>]
        member this.evalIngredient () =
            let input = [Ingredient("Orange")] @ [Ingredient("A voc a fresh a do")]
            let expected = @"\item " + "Orange"  + "\n" + @"\item " + "A voc a fresh a do"  + "\n"
            let result2 = evalIngredient (input)
            Assert.AreEqual(expected, result2)

