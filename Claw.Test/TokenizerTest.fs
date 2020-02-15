module Claw.Test.TokenizerTest

module Precedence = 
    open NUnit.Framework
    open NUnit.Framework.Constraints
    open Claw.Core.Tokenizer
    open Claw.Program

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
        let inputs = List.map indent [50..2] |> List.map (sprintf "%s#") |> List.rev
        Assert.Fail(sprintf "%A" inputs)

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