module Claw.Main

open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

[<EntryPoint>]
let main argv = 
    repl (fun () -> System.Console.ReadLine()) |> ignore
    stripWhiteSpace $ unpackMaybeParseResult (executeTokenParser "#define MYVALUE a  ")
    0