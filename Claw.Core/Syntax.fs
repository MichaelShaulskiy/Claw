(* Transforms a list of token into an AST *)
module Claw.Core.Syntax

open Claw.Core.TokenizerTypes

let withOption x f = match x with 
                     | Some xs -> f xs
                     | _ -> None

type AST<'T> =
    | Node of list<AST<'T>> * 'T * list<AST<'T>>

let buildAST (tokens: list<Token> option) = withOption 


