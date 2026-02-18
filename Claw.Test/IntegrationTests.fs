module Claw.Test.IntegrationTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

[<Test>]
let ``Complete function declaration`` () =
    let input = "int main(void) {"
    let expected = [
        Keyword Int
        Identifier "main"
        Punctuator LeftParen
        Keyword Void
        Punctuator RightParen
        Punctuator LeftBrace
    ]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Preprocessor with macro`` () =
    let input = "#define MAX 100"
    let expected = [
        Directive Define
        Identifier "MAX"
        Literal (IntegerLiteral (100L, None))
    ]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Variable assignment`` () =
    let input = "x = 42;"
    let expected = [
        Identifier "x"
        Operator Assign
        Literal (IntegerLiteral (42L, None))
        Punctuator Semicolon
    ]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Array access`` () =
    let input = "arr[0]"
    let expected = [
        Identifier "arr"
        Punctuator LeftBracket
        Literal (IntegerLiteral (0L, None))
        Punctuator RightBracket
    ]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Function call`` () =
    let input = "printf(\"Hello\");"
    let expected = [
        Identifier "printf"
        Punctuator LeftParen
        Literal (StringLiteral ("Hello", None))
        Punctuator RightParen
        Punctuator Semicolon
    ]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Complex expression`` () =
    let input = "a + b * c"
    let expected = [
        Identifier "a"
        Operator Plus
        Identifier "b"
        Operator Multiply
        Identifier "c"
    ]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)
