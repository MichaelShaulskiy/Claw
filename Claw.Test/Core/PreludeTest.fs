module Claw.Test.PreludeTest

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Core.Prelude


[<SetUp>]
let Setup () = 
    ()

[<Test>]
let ``infix Concat Carret`` () = 
    Assert.AreEqual("A" ^ "B", "AB", "Concat Carret fails with only 2 Parameters.")
    Assert.AreEqual("A" ^ "B" ^ "C", "ABC", "Concat Carret fails with 3 Parameters without parenthesis.")
    Assert.AreEqual("A" ^ ("B" ^ "C"), "ABC", "Concat Carret has improper Precedence.")
    Assert.AreEqual(("A" ^ "B") ^ "C", "A" ^ ("B" ^ "C"), "Concat Carret is not Associative.")
    Assert.AreNotEqual(("A" ^ "B"), ("B" ^ "A"), "Concat Carret is not Commutative.")
    Assert.AreNotEqual("A" ^ "", "A ", "Concat Carret appends unneccessary space.")

[<Test>]
let ``fold List of strings into a string`` () = 
    let input = [
                    "#"
                    "@"
                    ";"
                    " "
                    "["
                    "]"
                    "%"
                    "("
                    ")"
                    ","
                    "*"
                    "&"
                    "{"
                    "}"
                    "+"
                    "-"
                    "/"
                    ">"
                    "<"
                    "_"
                    "!"
                    "~"
                    "?"
                    ":"
                    "."
                  ]

    let expected = "#@; []%(),*&{}+-/><_!~?:."

    Assert.That(foldStringList input, Is.EqualTo(expected), "foldstringList doesn't work properly.")

[<Test>]
let ``Zip with Lists both length`` () = 
    // Both lists have the same length and type
    let expected = [(1, 1); (2,2); (3,3); (4,4)]
    let actual = zip [1..4] [1..4]
    Assert.That(actual, Is.EqualTo(expected))



    // Both lists empty

[<Test>]
let ``Zip with One List shorter`` () = 
    let expected = [(1, 1); (2,2); (3,3); (4,4)]
    // One List is shorter
    let actual = zip [1..5] [1..4]
    Assert.That(actual, Is.EqualTo(expected))

    let actual = zip [1..4] [1..5]
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let ``Zip with One Empty List`` () = 
    // One list is empty
    let actual = zip [] [1..4]
    Assert.That(actual, Is.EqualTo([]))

    let actual = zip [1..4] []
    Assert.That(actual, Is.EqualTo([]))

[<Test>]
let ``Zip with Both Lists empty`` () = 
    // One list is empty
    let actual = zip [] []
    Assert.That(actual, Is.EqualTo([]))