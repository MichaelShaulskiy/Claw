module Claw.Test.SyntaxExt

open NUnit.Framework

let inline (==) (actual: #obj) (expected: #obj) = Assert.AreEqual(expected, actual)
let inline (!=) (actual: #obj) (expected: #obj) = Assert.AreNotEqual(expected, actual)

