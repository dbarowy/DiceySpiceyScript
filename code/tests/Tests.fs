namespace RecipeTests
open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open code

[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member this.ingredient () =
        let input = "ing[lettuce, corn, beef, dorito chips]"
        let expected = [ "lettuce"; "corn"; "beef"; "dorito chips"]
        let result = parse (prepare input)
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

