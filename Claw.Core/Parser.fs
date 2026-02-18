module Claw.Core.Parser

open Claw.Core.Tokenizer
open Claw.Core.Prelude

// Everything is an expression, there are no statements

type SeqTree<'T> = 
    | SNode of 'T * seq<SeqTree<'T>>
    | SLeaf of 'T

type ParseTree<'T> = 
    | PNode of 'T * List<ParseTree<'T>>
    | PLeaf of 'T

type PExpression = 
    | DeclareStmt of Token * PExpression option //last is value

let pdirectivedefinesimple = fun (input : Token list) ->
    match input with
    | Directive Define :: Identifier x :: xs -> Some (Pleaf $ DeclareStmt (Directive Define))
    | [Directive Define] -> Some (PLeaf $ DeclareStmt (Directive Define, None))
    | _ -> None

let pdirectivedefineparse (input: Token list): ParseTree<PExpression> =
    match input with
    | [Directive Define; Identifier x] ->
        PLeaf $ DeclareStmt (Directive Define, Some $ DeclareStmt (Identifier x, None))
    | [Directive Define; Identifier x; value] ->
        PLeaf $ DeclareStmt (Directive Define, Some $ DeclareStmt (Identifier x, Some $ DeclareStmt (value, None)))
    | _ -> PLeaf $ DeclareStmt (Directive Define, None)

    

// let pdeclparse: ParseTree<Token> = 