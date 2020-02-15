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
    let ``All Char Operators correctly Parsed`` () =
        let input = "#"
        let expected = [Hash]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Hash could not be parsed.")

        let input = "@"
        let expected = [AtSign]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "AtSign could not be parsed.")

        let input = ";"
        let expected = [Semicolon]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Semicolon could not be parsed.")

        let input = " "
        let expected = [Space]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Space could not be parsed.")

        let input = "["
        let expected = [SquareBracketOpen]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "SquareBracketOpen could not be parsed.")

        let input = "]"
        let expected = [SquareBracketClose]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "SquareBracketClose could not be parsed.")

        let input = ","
        let expected = [Comma]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Comma could not be parsed.")

        let input = "*"
        let expected = [Asterisk]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Asterisk could not be parsed.")

        let input = "&"
        let expected = [Ampersand]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Ampersand could not be parsed.")

        let input = "{"
        let expected = [CurlyBraceOpen]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "CurlyBraceOpen could not be parsed.")

        let input = "}"
        let expected = [CurlyBraceClose]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "CurlyBraceClose could not be parsed.")

        let input = "+"
        let expected = [Plus]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Plus could not be parsed.")

        let input = "-"
        let expected = [Minus]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Minus could not be parsed.")
        
        // TODO: Check comment test
        let input = "/"
        let expected = [Div]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Div could not be parsed.")

        let input = ">"
        let expected = [Greater]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Greater could not be parsed.")

        let input = "<"
        let expected = [Less]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Less could not be parsed.")

        let input = "_"
        let expected = [Underscore]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Underscore could not be parsed.")

        let input = "!"
        let expected = [LogicalNot]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "LogicalNot could not be parsed.")

        let input = "~"
        let expected = [Complement]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Complement could not be parsed.")

        let input = "?"
        let expected = [QuestionMark]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "QuestionMark could not be parsed.")

        let input = ":"
        let expected = [Colon]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Colon could not be parsed.")

        //TODO: VarArgs choice test
        let input = "."
        let expected = [Dot]
        let actual = parserGetResultWithWhitespace input
        Assert.That(actual, Is.EqualTo(expected), "Dot could not be parsed.")

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