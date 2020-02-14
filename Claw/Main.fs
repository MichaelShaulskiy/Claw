module Claw.Main

open Claw.Core.Tokenizer

[<EntryPoint>]
let main argv = 
    executeTokenParser "#define MYVALUE a"
    |> (function Some xs -> xs | None -> [])
    |> printfn "%A"
    0