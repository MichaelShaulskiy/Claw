module Claw.Test.CommentTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

[<Test>]
let ``Single line comment`` () =
    let input = "// This is a comment"
    let expected = [Comment "// This is a comment"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Multi line comment`` () =
    let input = "/* This is a comment */"
    let expected = [Comment "/*This is a comment */"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)
