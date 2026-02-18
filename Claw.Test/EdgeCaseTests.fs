module Claw.Test.EdgeCaseTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

[<Test>]
let ``Empty input`` () =
    let input = ""
    let expected = []
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Only whitespace`` () =
    let input = "   \t  "
    let expected = []
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Mixed tokens with whitespace`` () =
    let input = "int x = 5;"
    let expectedWithoutWhitespace = [
        Keyword Int
        Identifier "x"
        Operator Assign
        Literal (IntegerLiteral (5L, None))
        Punctuator Semicolon
    ]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expectedWithoutWhitespace, actual)

[<Test>]
let ``Very long identifier`` () =
    let longId = String.replicate 100 "a"
    let input = longId
    let expected = [Identifier longId]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Unicode in comments`` () =
    let input = "// Hello 世界"
    let expected = [Comment "// Hello 世界"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)
