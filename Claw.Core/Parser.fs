module Claw.Core.Parser

open Claw.Core.Tokenizer

// Everything is an expression, there are no statements

type SeqTree<'T> = 
    | SNode of 'T * seq<SeqTree<'T>>
    | SLeaf of 'T

type ParseTree<'T> = 
    | PNode of 'T * List<ParseTree<'T>>
    | PLeaf of 'T

type PExpression = 
    | DeclareStmt

let pdirectivedefineparse (input: Token list): ParseTree<PExpression> =
    let pdefineboolimpl = fun tokens -> match tokens with
                                            | Directive Define Identifier ident :: [] ->   
    match input with
    | Directive Define :: xs -> PLeaf (DeclareStmt)

// let pdeclparse: ParseTree<Token> = 