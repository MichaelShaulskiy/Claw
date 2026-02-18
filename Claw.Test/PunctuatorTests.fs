module Claw.Test.PunctuatorTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

[<Test>]
let ``Basic punctuators`` () =
    let testCases = [
        ("(", LeftParen)
        (")", RightParen)
        ("[", LeftBracket)
        ("]", RightBracket)
        ("{", LeftBrace)
        ("}", RightBrace)
        (";", Semicolon)
        (",", Comma)
        (":", Colon)
        ("#", Hash)
        ("_", Underscore)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Punctuator expected], actual)

[<Test>]
let ``Multi-character punctuators`` () =
    let testCases = [
        ("->", SingleArrowRight)
        ("<-", SingleArrowLeft)
        ("...", Ellipsis)
        ("##", DoubleHash)
        ("::", DoubleColon)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Punctuator expected], actual)
