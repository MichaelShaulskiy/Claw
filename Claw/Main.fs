module Claw.Main

open Claw.Core.Tokenizer
open Claw.Program

[<EntryPoint>]
let main argv = 
    repl (fun () -> printfn "Claw REPL"
                    System.Console.ReadLine()) |> ignore
    executeTokenParser "#define MYVALUE a"
    |> (function Some xs -> xs | None -> [])
    |> printfn "%A"
    0