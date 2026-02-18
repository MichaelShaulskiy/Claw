module Claw.Test.DirectiveTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)
let parserGetResultWithWhitespace = tokenize

[<Test>]
let ``#define without identifier or value`` () = 
    let input = "#define"
    let expected = [Directive Define]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#define with valid identifiers but no value`` () = 
    let input = "#define a"
    let expected = [Directive Define; Whitespace " "; Identifier "a"]
    let actual = parserGetResultWithWhitespace input |> List.filter (fun x -> x <> EOF)
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#undef directive`` () =
    let input = "#undef MACRO_NAME"
    let expected = [Directive Undef; Identifier "MACRO_NAME"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#include directive`` () =
    let input = "#include"
    let expected = [Directive Include]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#ifdef directive`` () =
    let input = "#ifdef DEBUG"
    let expected = [Directive IfDef; Identifier "DEBUG"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#ifndef directive`` () =
    let input = "#ifndef HEADER_H"
    let expected = [Directive IfNDef; Identifier "HEADER_H"]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#if directive`` () =
    let input = "#if"
    let expected = [Directive IfDirective]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#else directive`` () =
    let input = "#else"
    let expected = [Directive ElseDirective]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#elif directive`` () =
    let input = "#elif"
    let expected = [Directive Elif]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#endif directive`` () =
    let input = "#endif"
    let expected = [Directive EndIf]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#error directive`` () =
    let input = "#error"
    let expected = [Directive Error]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#pragma directive`` () =
    let input = "#pragma"
    let expected = [Directive Pragma]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#line directive`` () =
    let input = "#line"
    let expected = [Directive Line]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``#warning directive`` () =
    let input = "#warning"
    let expected = [Directive Warning]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Mixin directive Begin @/*`` () =
    let input = "@/*"
    let expected = [Directive MixinBegin]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)

[<Test>]
let ``Mixin directive End */@`` () =
    let input = "*/@"
    let expected = [Directive MixinEnd]
    let actual = parserGetResult input
    CollectionAssert.AreEqual(expected, actual)
