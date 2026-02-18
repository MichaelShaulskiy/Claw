module Claw.Test.PreprocessorOperatorTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

[<Test>]
let ``defined operator`` () =
    let input = "defined"
    let expected = [PreprocessorOperator Defined]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``__has_include operator`` () =
    let input = "__has_include"
    let expected = [PreprocessorOperator HasInclude]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``__has_attribute operator`` () =
    let input = "__has_attribute"
    let expected = [PreprocessorOperator HasAttribute]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)
