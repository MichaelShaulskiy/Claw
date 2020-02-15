module Claw.Test.TokenizerTest

module Precedence = 
    open NUnit.Framework
    open NUnit.Framework.Constraints
    open Claw.Core.Tokenizer
    open Claw.Program
    open Claw.Core.Prelude
    
    let parserGetResult = executeTokenParser >> unpackMaybeParseResult >> stripWhiteSpace

    let parserGetResultWithWhitespace = executeTokenParser >> unpackMaybeParseResult

    [<SetUp>]
    let Setup () = 
        ()

    [<Test>]
    let ``phash Simple Case`` () = 
        let input = "#"
        let expected = Some [Hash]
        let actual = executeTokenParser input
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    let ``hash preceeded by whitespace`` () =
        let expected = [Hash]
        List.map (fun x -> new string(' ', x) ^ "#") [1..50]
        |> List.map (executeTokenParser >> unpackMaybeParseResult >> stripWhiteSpace)
        |> List.iter (fun x -> Assert.That(x, Is.EqualTo(expected), "hash is not recognized when preceeded by whitespace."))

    [<Test>]
    let ``All Two Char correctly Parsed`` () =
        let input = "%="
        let expected = [ModuloAsign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "ModuloAssign could not be parsed.")

        let input = "=="
        let expected = [Eq]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Eq could not be parsed.")

        let input = "&&"
        let expected = [LogicalAnd]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "LogicAnd could not be parsed.")

        let input = @"//"
        let expected = [SingleLineComment]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "SingleLineComment could not be parsed.")

        let input = @"/*"
        let expected = [MultiLineCommentStart]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "MultiLineCommentStart could not be parsed.")

        let input = @"*/"
        let expected = [MultiLineCommentStop]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "MultiLineCommentStop could not be parsed.")

        let input = "++"
        let expected = [Increment]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Increment could not be parsed.")

        let input = "--"
        let expected = [Decrement]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Decrement could not be parsed.")

        let input = "->"
        let expected = [Arrow]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Arrow could not be parsed.")

        let input = "+="
        let expected = [IncrementAsign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "IncrementAsign could not be parsed.")

        let input = "-="
        let expected = [DecrementAsign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "DecrementAsign could not be parsed.")

        let input = "&="
        let expected = [BitwiseAndAsing]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "BitwiseAndAsing could not be parsed.")

        let input = "^="
        let expected = [BitwiseXOrAsign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "BitwiseXOrAsign could not be parsed.")

        let input = "|="
        let expected = [BitwiseORAsign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "BitwiseORAsign could not be parsed.")

        let input = "*="
        let expected = [MulAsign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "MulAsign could not be parsed.")

        let input = "/="
        let expected = [DivAsign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "DivAsign could not be parsed.")

        let input = "!="
        let expected = [NEq]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "NEq could not be parsed.")

        let input = "<="
        let expected = [LessEq]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "LessEq could not be parsed.")

        let input = ">="
        let expected = [GreaterEq]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "GreaterEq could not be parsed.")

        let input = "<<"
        let expected = [ShiftLeft]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "ShiftLeft could not be parsed.")

        let input = ">>"
        let expected = [ShiftRight]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "ShiftRight could not be parsed.")

        let input = "::"
        let expected = [ScopeOp]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "ScopeOp could not be parsed.")

        let input = "||"
        let expected = [LogicalOr]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "LogicalOr could not be parsed.")

        let input = "``"
        let expected = [BackTick]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "BackTick could not be parsed.")

    [<Test>]
    let ``All Three Char correctly Parsed`` () =
        let input = "<<="
        let expected = [ShiftLeftAsign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "ShiftLeftAsign could not be parsed.")

        let input = ">>="
        let expected = [ShiftRightAsign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "ShiftRightAsign could not be parsed.")

        let input = "..."
        let expected = [VarArgs]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "VarArgs could not be parsed.")

    [<Test>]
    let ``Colon or Scope`` () =
        let input = ":"
        let expected = [Colon]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Colon not recognized.")

        let input = "::"
        let expected = [ScopeOp]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "ScopeOp not recognized.")

        let input = ": :"
        let expected = [Colon; Colon]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Colon followed by Colon.")

        let input = ": ::"
        let expected = [Colon; ScopeOp]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Colon followed by ScopeOp.")

        let input = ":: :"
        let expected = [ScopeOp; Colon]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "ScopeOp followed by Colon")

    [<Test>]
    let ``Ampersand or LAnd`` () =
        let input = "&"
        let expected = [Ampersand]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Ampersand not recognized.")

        let input = "&&"
        let expected = [LogicalAnd]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "LogicalAnd not recognized.")

        let input = "& &"
        let expected = [Ampersand; Ampersand]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Ampersand followed by Ampersand")


    [<Test>]
    let ``Dot Or VarArgs`` () =
        let input = @"."
        let expected = [Dot]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Parser cannot detect a single dot.")

        let input = @".."
        let expected = [Dot; Dot]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Parser cannot distinguish two dots.")

        let input = @". ."
        let expected = [Dot; Dot]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Parser cannot distinguish two dots.")

        let input = @"..."
        let expected = [VarArgs]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Parser cannot distinguish three dots from VarArgs.")

        let input = @"... ."
        let expected = [VarArgs; Dot]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "VarArgs followed by Dot.")

        let input = @"... .."
        let expected = [VarArgs; Dot; Dot]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "VarArgs followed by 2 Dots.")

        let input = @"... .. ."
        let expected = [VarArgs; Dot; Dot; Dot]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "VarArgs followed by 3 Dots.")


    [<Test>]
    let ``distinguish comments or div`` () = 
        let input = @"/"
        let expected = [Div]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Parser cannot distinguish single Slash.")

        let input = @"//"
        let expected = [SingleLineComment]
        let actual = parserGetResult input
        Assert.AreEqual(expected, actual, "Parser cannot distinguish SingleLineComment")

        let input = @"/*"
        let expected = [MultiLineCommentStart]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Parser cannot detect the beginning of a multiline comment.")

        let input = @"*/"
        let expected = [MultiLineCommentStop]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Parser cannot detect the end of a multiline comment.")

    [<Test>]
    let ``tricky comments or div`` () = 
        let input = @"2 / //"
        let expected = [IntLiteral 2L; Div; SingleLineComment]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Div followed by single line comment.")

        let input = @"2/ //"
        let expected = [IntLiteral 2L; Div; SingleLineComment]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Div followed by single line comment.")

        let input = @"2 ///"
        let expected = [IntLiteral 2L; SingleLineComment]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Div followed by single line docstring.")

        let input = @"2 / /*"
        let expected = [IntLiteral 2L; Div; MultiLineCommentStart]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Div followed by MultilineCommentStart.")

        let input = @"2 / */"
        let expected = [IntLiteral 2L; Div; MultiLineCommentStop]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Div followed by MultilineCommentStop.")

        let input = @"2 / /**/"
        let expected = [IntLiteral 2L; Div; MultiLineCommentStart; MultiLineCommentStop]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Div followed by enclosed MultiLineComment on single line.")

        let input = @"2 / /* * */"
        let expected = [IntLiteral 2L; Div; MultiLineCommentStart; Asterisk; MultiLineCommentStop]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Div followed by Multilinecomment encasing a single asterisk")

        let input = @"2 / /* // */"
        let expected = [IntLiteral 2L; Div; MultiLineCommentStart; SingleLineComment; MultiLineCommentStop]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "Div followed by MultilineComment encasing single line comment.")


    [<Test>]
    let ``phash distinguish from other hash beginnings`` () = 
        let input = ["#define"; "#ifndef"]
        let expected = [Some [Define]; Some [IfNDef]]
        let actual = input |> List.map executeTokenParser
        Assert.Fail()

module General = 
    open NUnit.Framework
    open NUnit.Framework.Constraints
    open Claw.Core.Tokenizer

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let ``Tokenize #define MYVALUE`` () =
        let input = "#define MYVALUE"
        let expected = Some [Define; Space; Identifier "MYVALUE"]
        let actual = executeTokenParser input
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    let ``Tokenize #define MYVALUE with one trailing space`` () =
        let input = "#define MYVALUE "
        let expected = Some [Define; Space; Identifier "MYVALUE"; Space]
        let actual = executeTokenParser input
        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    let ``Tokenize #define MYVALUE with multiple trailing spaces`` () =
        let input = "#define MYVALUE  "
        let expected = Some [Define; Space; Identifier "MYVALUE"; Space; Space;]
        let actual = executeTokenParser input
        Assert.That(actual, Is.EqualTo(expected))


    [<Test>]
    let ``Tokenize #define _MYVALUE`` () = 
        let input = "#define _MYVALUE"
        let expected = Some [Define; Space; Identifier "_MYVALUE"]
        let actual = executeTokenParser input

        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    let ``Tokenize #define _MY_VALUE`` () = 
        let input = "#define _MY_VALUE"
        let expected = Some [Define; Space; Identifier "_MY_VALUE"]
        let actual = executeTokenParser input

        Assert.That(actual, Is.EqualTo(expected))


    [<Test>]
    let ``Tokenize #define _MY_VALUE_`` () = 
        let input = "#define _MY_VALUE_"
        let expected = Some [Define; Space; Identifier "_MY_VALUE_"]
        let actual = executeTokenParser input

        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    let ``Tokenize #define MYMACRO(a)`` () = 
        let input = "#define MYMACRO(a)"
        let expected = Some [Define; Space; Identifier "MYMACRO"; ParenOpen; Identifier "a"; ParenClose]
        let actual = executeTokenParser input

        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    let ``Tokenize #define MYMACRO(a,b)`` () = 
        let input = "#define MYMACRO(a,b)"
        let expected = Some [Define; Space; Identifier "MYMACRO"; ParenOpen; Identifier "a"; Comma; Identifier "b"; ParenClose]
        let actual = executeTokenParser input

        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    let ``Tokenize #define MYMACRO(a, b)`` () = 
        let input = "#define MYMACRO(a, b)"
        let expected = Some [Define; Space; Identifier "MYMACRO"; ParenOpen; Identifier "a"; 
                             Comma; Space; Identifier "b"; ParenClose]
        let actual = executeTokenParser input

        Assert.That(actual, Is.EqualTo(expected))

    [<Test>]
    let ``Tokenize #define MYMACRO(a, b) with one trailing space`` () = 
        let input = "#define MYMACRO(a, b) "
        let expected = Some [Define; Space; Identifier "MYMACRO"; ParenOpen; Identifier "a"; 
                             Comma; Space; Identifier "b"; ParenClose; Space]
        let actual = executeTokenParser input

        Assert.That(actual, Is.EqualTo(expected))

module Literals = 
    open NUnit.Framework
    open NUnit.Framework.Constraints
    open Claw.Core.Tokenizer
    open Claw.Program
    open Claw.Core.Prelude
    
    let parserGetResult = executeTokenParser >> unpackMaybeParseResult >> stripWhiteSpace

    let parserGetResultWithWhitespace = executeTokenParser >> unpackMaybeParseResult

    [<Test>]
    let ``One Digit Positive Int Literal`` () =
        let inputs = List.map string [0..9]
        let expected = [IntLiteral 0L, IntLiteral 1L, IntLiteral 2L, IntLiteral 3L, IntLiteral 4L, IntLiteral 5L, IntLiteral 6L, IntLiteral 7L, IntLiteral 8L, IntLiteral 9L]
        let actual = List.map parserGetResult inputs |> List.concat
        Assert.That(actual, Is.EqualTo(expected), "IntLiterals cannot be parsed.")

    [<Test>]
    let ``Char Literal`` () = 
        let input = @"'a'"
        let expected = [CharLiteral 'a']
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected))

module FundamentalTypes = 
    open NUnit.Framework
    open NUnit.Framework.Constraints
    open Claw.Core.Tokenizer
    open Claw.Program
    open Claw.Core.Prelude

    let parserGetResult = executeTokenParser >> unpackMaybeParseResult >> stripWhiteSpace

    let parserGetResultWithWhitespace = executeTokenParser >> unpackMaybeParseResult

    [<Test>]
    let ``Void Type`` () = 
        let input = "void"
        let expected = [Void]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "void not recognized.")

        let input = "(void)"
        let expected = [ParenOpen; Void; ParenClose]
        let actual = parserGetResult input
        Assert.That(actual, Is.EqualTo(expected), "void cast.")

    [<Test>]
    let ``bool type`` () =
        Assert.Fail()


    [<Test>]
    let ``Fundamental Pointer Type`` () = 
        Assert.Fail()

module SourceCode = 
    open NUnit.Framework
    open NUnit.Framework.Constraints
    open Claw.Core.Tokenizer
    open Claw.Program
    open Claw.Core.Prelude

    let parserGetResult = executeTokenParser >> unpackMaybeParseResult >> stripWhiteSpace

    let parserGetResultWithWhitespace = executeTokenParser >> unpackMaybeParseResult

    [<Test>]
    let ``Variable Declaration without Assignment`` () = 
        Assert.Fail()

    [<Test>]
    let ``Function Declaration Without Definition`` () =
        Assert.Fail()

    [<Test>]
    let ``Typedef`` () = 
        Assert.Fail()

    [<Test>]
    let ``Type Alias using Using Keyword`` () =
        Assert.Fail()

    [<Test>]
    let ``sizeof expression`` () =
        Assert.Fail()

    [<Test>]
    let ``const Keyword`` () = 
        Assert.Fail()
    
        