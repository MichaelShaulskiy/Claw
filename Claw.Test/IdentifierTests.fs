module Claw.Test.IdentifierTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

[<Test>]
let ``Simple identifier`` () =
    let input = "variable"
    let expected = [Identifier "variable"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Identifier with underscore`` () =
    let input = "_private_var"
    let expected = [Identifier "_private_var"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Identifier with numbers`` () =
    let input = "var123"
    let expected = [Identifier "var123"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Multiple identifiers`` () =
    let input = "var1 var2"
    let expected = [Identifier "var1"; Identifier "var2"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)
