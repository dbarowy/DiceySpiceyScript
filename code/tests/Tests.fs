namespace RecipeTests
open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open Evaluator
open AST

[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member this.ingredient () =
        let input = "ing[lettuce, corn, beef, chips]"
        let expected = [ [Instruction("lettuce")] ; [Instruction("corn")] ; [Instruction("beef")] ; [Instruction("chips")]]
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    // [<TestMethod>]
    //     member this.evalIngredient () =
    //         let input = "ing[lettuce, corn, beef, chips]"
    //         let expected = [ @"\item lettuce \n"; @"\item corn \n"; @"\item beef \n"; @"\item chips \n"]
    //         let result = parse (input)
    //         match result with
    //         | Some ws ->
    //             Assert.AreEqual(expected, ws)
    //         | None ->
    //             Assert.IsTrue false

