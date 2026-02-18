module Claw.Test.KeywordTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

[<Test>]
let ``Storage class specifiers`` () =
    let testCases = [
        ("auto", Auto)
        ("register", Register) 
        ("static", Static)
        ("extern", Extern)
        ("_Thread_local", ThreadLocal)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Keyword expected], actual)

[<Test>]
let ``Type specifiers`` () =
    let testCases = [
        ("void", Void)
        ("char", Char)
        ("short", Short)
        ("int", Int)
        ("long", Long)
        ("float", Float)
        ("double", Double)
        ("signed", Signed)
        ("unsigned", Unsigned)
        ("_Bool", Bool)
        ("_Complex", Complex)
        ("_Imaginary", Imaginary)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Keyword expected], actual)

[<Test>]
let ``Type qualifiers`` () =
    let testCases = [
        ("const", Const)
        ("restrict", Restrict)
        ("volatile", Volatile)
        ("_Atomic", Atomic)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Keyword expected], actual)

[<Test>]
let ``Function specifiers`` () =
    let testCases = [
        ("inline", Inline)
        ("_Noreturn", NoReturn)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Keyword expected], actual)

[<Test>]
let ``Control flow keywords`` () =
    let testCases = [
        ("if", If)
        ("else", Else)
        ("switch", Switch)
        ("case", Case)
        ("default", Default)
        ("while", While)
        ("for", For)
        ("do", Do)
        ("break", Break)
        ("continue", Continue)
        ("goto", Goto)
        ("return", Return)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Keyword expected], actual)

[<Test>]
let ``Other keywords`` () =
    let testCases = [
        ("struct", Struct)
        ("union", Union)
        ("enum", Enum)
        ("typedef", Typedef)
        ("_Static_assert", StaticAssert)
        ("_Alignas", AlignAs)
    ]
    for (input, expected) in testCases do
        let actual = parserGetResult input
        CollectionAssert.AreEqual([Keyword expected], actual)
