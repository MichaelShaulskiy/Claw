module Claw.Test.HeaderNameTests

open NUnit.Framework
open NUnit.Framework.Constraints
open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

// Helper functions for testing
let parserGetResult = tokenize >> List.filter (fun x -> x <> EOF)

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
