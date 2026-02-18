module Claw.Test.WhitespaceTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)
let parserGetResultWithWhitespace = tokenize

[<Test>]
let ``Whitespace preserved`` () =
    let input = "   "
    let actual = parserGetResultWithWhitespace input |> List.filter (fun x -> x <> EOF)
    let expected = [Whitespace "   "]
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Newline`` () =
    let input = "\n"
    let actual = parserGetResultWithWhitespace input |> List.filter (fun x -> x <> EOF)
    let expected = [Newline]
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Tab whitespace`` () =
    let input = "\t\t"
    let actual = parserGetResultWithWhitespace input |> List.filter (fun x -> x <> EOF)
    let expected = [Whitespace "\t\t"]
    CollectionAssert.AreEqual(expected, actual)
