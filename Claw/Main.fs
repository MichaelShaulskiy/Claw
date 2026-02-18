module Claw.Main

open Claw.Core.Tokenizer
open Claw.Program
open Claw.Core.Prelude

[<EntryPoint>]
let main argv = 
    
    // Original REPL functionality
    printfn "\nStarting REPL (type EXIT to quit)..."
    repl (fun () -> System.Console.ReadLine()) |> ignore
    0