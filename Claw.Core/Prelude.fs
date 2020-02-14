(* Implementing some Haskell functions *)
module Claw.Core.Prelude

open System
open System.IO

let readFile filePath = List.ofSeq (File.ReadAllLines(filePath))
let writeFile filePath str = 
    use fs = new FileStream(filePath, FileMode.CreateNew, FileAccess.Write)
    fs.Write(str, 0, str.Length)

(* haskell lines and unlines *)
let (^) l r = sprintf "%s%s" l r

let rec zip (xs: 'a list) (ys : 'b list) : (('a * 'b) list) = match xs with
                                                              | xshead :: xstail -> match ys with
                                                                                    | yshead :: ystail -> (xshead, yshead)::(zip xstail ystail)
                                                                                    | [] -> []
                                                              | [] -> []