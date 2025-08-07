module Claw.Core.Parser

open Claw.Core.Tokenizer

// Everything is an expression, there are no statements

type SeqTree<'T> = 
    | SNode of 'T * seq<SeqTree<'T>>
    | SLeaf of 'T

type ParseTree<'T> = 
    | PNode of 'T * List<ParseTree<'T>>
    | PLeaf of 'T

let pdeclparse: ParseTree<Token> = 
