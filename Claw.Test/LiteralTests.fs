module Claw.Test.LiteralTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

[<Test>]
let ``Integer literals decimal`` () =
    let input = "123"
    let expected = [Literal (IntegerLiteral (123L, None))]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Integer literals with suffix`` () =
    let testCases = [
        ("123u", 123L, Some "u")
        ("123L", 123L, Some "L") 
        ("123UL", 123L, Some "UL")
        ("123ll", 123L, Some "ll")
    ]
    for (input, expectedValue, expectedSuffix) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Literal (IntegerLiteral (expectedValue, expectedSuffix))], actual)

[<Test>]
let ``Hexadecimal integer literals`` () =
    let input = "0xFF"
    let expected = [Literal (IntegerLiteral (255L, None))]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Octal integer literals`` () =
    let input = "0755"
    let expected = [Literal (IntegerLiteral (493L, None))]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Floating point literals`` () =
    let input = "3.14"
    let expected = [Literal (FloatingLiteral (3.14, None))]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Floating point with suffix`` () =
    let input = "3.14f"
    let expected = [Literal (FloatingLiteral (3.14, Some "f"))]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Character literals`` () =
    let input = "'a'"
    let expected = [Literal (CharacterLiteral ('a', None))]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Character literals with prefix`` () =
    let input = "L'a'"
    let expected = [Literal (CharacterLiteral ('a', Some "L"))]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Escaped character literals`` () =
    let testCases = [
        ("'\\n'", '\n')
        ("'\\t'", '\t')
        ("'\\r'", '\r')
        ("'\\\\'", '\\')
        ("'\\''", '\'')
    ]
    for (input, expectedChar) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Literal (CharacterLiteral (expectedChar, None))], actual)

[<Test>]
let ``String literals`` () =
    let input = "\"hello\""
    let expected = [Literal (StringLiteral ("hello", None))]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``String literals with prefix`` () =
    let testCases = [
        ("L\"hello\"", "hello", Some "L")
        ("u\"hello\"", "hello", Some "u")
        ("U\"hello\"", "hello", Some "U")
        ("u8\"hello\"", "hello", Some "u8")
    ]
    for (input, expectedStr, expectedPrefix) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Literal (StringLiteral (expectedStr, expectedPrefix))], actual)

[<Test>]
let ``String literals with escape sequences`` () =
    let input = "\"hello\\nworld\""
    let expected = [Literal (StringLiteral ("hello\nworld", None))]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)
