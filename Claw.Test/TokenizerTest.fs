module Claw.Test.TokenizerTest

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)
let parserGetResultWithWhitespace = tokenize

[<SetUp>]
let Setup () = 
    ()

// ===== PREPROCESSOR DIRECTIVES TESTS =====
module DirectiveTests =
    
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

// ===== PREPROCESSOR OPERATORS TESTS =====
module PreprocessorOperatorTests =

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

// ===== KEYWORDS TESTS =====
module KeywordTests =

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

// ===== IDENTIFIERS TESTS =====
module IdentifierTests =

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

// ===== LITERAL TESTS =====
module LiteralTests =

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

// ===== OPERATOR TESTS =====
module OperatorTests =

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

// ===== PUNCTUATOR TESTS =====
module PunctuatorTests =

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
        ]
        for (input, expected) in testCases do
            let actual = parserGetResult input
            CollectionAssert.AreEqual([Punctuator expected], actual)

    [<Test>]
    let ``Multi-character punctuators`` () =
        let testCases = [
            ("...", Ellipsis)
            ("##", DoubleHash)
        ]
        for (input, expected) in testCases do
            let actual = parserGetResult input
            CollectionAssert.AreEqual([Punctuator expected], actual)

// ===== HEADER NAME TESTS =====
module HeaderNameTests =

    [<Test>]
    let ``System header`` () =
        let input = "<stdio.h>"
        let expected = [HeaderName ("stdio.h", true)]
        let actual = parserGetResult input
        CollectionAssert.AreEqual(expected, actual)

    [<Test>]
    let ``Local header`` () =
        let input = "\"myheader.h\""
        let expected = [HeaderName ("myheader.h", false)]
        let actual = parserGetResult input
        CollectionAssert.AreEqual(expected, actual)

// ===== COMMENT TESTS =====
module CommentTests =

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

// ===== WHITESPACE AND NEWLINE TESTS =====
module WhitespaceTests =

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

// ===== COMPLEX INTEGRATION TESTS =====
module IntegrationTests =

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

// ===== EDGE CASES AND ERROR HANDLING =====
module EdgeCaseTests =

    [<Test>]
    let ``Empty input`` () =
        let input = ""
        let expected = []
        let actual = parserGetResult input
        CollectionAssert.AreEqual(expected, actual)

    [<Test>]
    let ``Only whitespace`` () =
        let input = "   \t  "
        let expected = []
        let actual = parserGetResult input
        CollectionAssert.AreEqual(expected, actual)

    [<Test>]
    let ``Mixed tokens with whitespace`` () =
        let input = "int x = 5;"
        let expectedWithoutWhitespace = [
            Keyword Int
            Identifier "x"
            Operator Assign
            Literal (IntegerLiteral (5L, None))
            Punctuator Semicolon
        ]
        let actual = parserGetResult input
        CollectionAssert.AreEqual(expectedWithoutWhitespace, actual)

    [<Test>]
    let ``Very long identifier`` () =
        let longId = String.replicate 100 "a"
        let input = longId
        let expected = [Identifier longId]
        let actual = parserGetResult input
        CollectionAssert.AreEqual(expected, actual)

    [<Test>]
    let ``Unicode in comments`` () =
        let input = "// Hello 世界"
        let expected = [Comment "// Hello 世界"]
        let actual = parserGetResult input
        CollectionAssert.AreEqual(expected, actual)
