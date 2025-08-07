module Claw.Program

open System
open FParsec
open Claw.Core.Prelude
open Claw.Core.Tokenizer

let rec manySpaces level = match level with
                           | 0 -> ""
                           | _ -> (sprintf "%s" " ") ^ (manySpaces (level - 1))

type SeqTree<'T> = 
    | SNode of 'T * seq<SeqTree<'T>>
    | SLeaf of 'T
    
let swapArguments f a b = f b a

let rec indent level = match level with
                       | 0 -> ""
                       | _ -> " " ^ (indent (level - 2))

let printIndented (xs: string list) = 
    let dat = [0..(xs.Length)] |> zip xs |> List.map (fun (value, level) -> sprintf "%s%s" (indent (level * 2)) value)
    List.iter (printfn "%A") dat


let repl (getInput : unit -> string) = 
    let mutable input = ""
    while not (input.ToUpper().Equals("EXIT")) do
        printf ":> "
        input <- getInput()

        //printfn "%A" (stripWhiteSpace (unpackMaybeParseResult (executeTokenParser input)))
        printfn "%A" (input |> tokenize |> stripWhiteSpace)
        

    0 // return an integer exit code
