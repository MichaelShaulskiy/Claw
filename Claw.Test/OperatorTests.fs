module Claw.Test.OperatorTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

[<Test>]
let ``Arithmetic operators`` () =
    let testCases = [
        ("+", Plus)
        ("-", Minus)
        ("*", Multiply)
        ("/", Divide)
        ("%", Modulo)
        ("++", Increment)
        ("--", Decrement)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Operator expected], actual)

[<Test>]
let ``Assignment operators`` () =
    let testCases = [
        ("=", Assign)
        ("+=", PlusAssign)
        ("-=", MinusAssign)
        ("*=", MultiplyAssign)
        ("/=", DivideAssign)
        ("%=", ModuloAssign)
        ("<<=", LeftShiftAssign)
        (">>=", RightShiftAssign)
        ("&=", BitwiseAndAssign)
        ("|=", BitwiseOrAssign)
        ("^=", BitwiseXorAssign)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Operator expected], actual)

[<Test>]
let ``Comparison operators`` () =
    let testCases = [
        ("==", Equal)
        ("!=", NotEqual)
        ("<", Less)
        (">", Greater)
        ("<=", LessEqual)
        (">=", GreaterEqual)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Operator expected], actual)

[<Test>]
let ``Logical operators`` () =
    let testCases = [
        ("&&", LogicalAnd)
        ("||", LogicalOr)
        ("!", LogicalNot)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Operator expected], actual)

[<Test>]
let ``Bitwise operators`` () =
    let testCases = [
        ("&", BitwiseAnd)
        ("|", BitwiseOr)
        ("^", BitwiseXor)
        ("~", BitwiseNot)
        ("<<", LeftShift)
        (">>", RightShift)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Operator expected], actual)

[<Test>]
let ``Other operators`` () =
    let testCases = [
        ("?", Conditional)
        ("->", Arrow)
        (".", Dot)
        ("::", ScopeResolution)
        ("sizeof", SizeOf)
        ("_Alignof", AlignOf)
        ("typeof", TypeOf)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Operator expected], actual)
