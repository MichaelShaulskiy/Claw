module Claw.Core.Tokenizer

open System
open FParsec
open System.Reflection
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core.LanguagePrimitives

type ParserState = unit
type Parser<'T> = Parser<'T, ParserState>

type Token = 
    | NOP
    | Semicolon
    | Hash
    | Define
    | Identifier of string
    | Space
    | IntLiteral of int64
    | StringLiteral of string
    | CharLiteral of char
    | Unsigned
    | SizeOf
    | SquareBracketOpen
    | SquarBracketClose
    | Percent
    | ParenOpen
    | ParenClose
    | Comma
    | Asterisk
    | Ampersand
    | CurlyBraceOpen
    | CurlyBraceClose
    | Plus
    | Minus
    | Div
    | Mul
    | Eq
    | NEq
    | Greater
    | Less
    | GreaterEq
    | LessEq
    | IfDef
    | IfNDef
    | Elif
    | Else
    | EndIf
    | If
    | Include
    | LineBreak
    | TError
    | Line
    | Pragma
    | And
    | Or
    | Not
    | QuestionMark
    | Colon
    | VarArgs
    
let stringToken s tret: Parser<_> = pstring s >>. preturn tret

let (<!>) (p: Parser<_,_>) label : Parser<_,_> = 
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leving %s (%A)" stream.Position label reply.Status
        reply
 
let str s = pstring s
let phash: Parser<_> = pchar '#' >>. preturn Hash
let pcomma: Parser<_> = pchar ',' >>. preturn Comma
let pplus: Parser<_> = pchar '+' >>. preturn Plus
let pminus: Parser<_> = pchar '-' >>. preturn Minus
let pdiv: Parser<_> = pchar '/' >>. preturn Div
let pmul: Parser<_> = pchar '*' >>. preturn Mul
let peq: Parser<_> = pstring "==" >>. preturn Eq
let pneq: Parser<_> = pstring "!=" >>. preturn NEq
let pgreater: Parser<_> = pstring ">" >>. preturn Greater
let pless: Parser<_> = pstring "<" >>. preturn Less
let pgreatereq: Parser<_> = pstring ">=" >>. preturn GreaterEq
let plesseq: Parser<_> = pstring "<=" >>. preturn LessEq
let pparenopen: Parser<_> = pchar '(' >>. preturn ParenOpen
let pparenclose: Parser<_> = pchar ')' >>. preturn ParenClose
let pspace: Parser<_> = pchar ' ' >>. preturn Space
let pvarargs: Parser<_> = pstring "..." >>. preturn VarArgs
let pdefine: Parser<_> = pstring "#define" >>. preturn Define
let pidentifier: Parser<_> = 
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    (many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier") |>> Identifier

let pintliteral: Parser<_> = pint64 |>> IntLiteral 
let pstringliteral = 
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c -> string c)
    between (pstring "\"") (pstring "\"")
                    (manyStrings (normalCharSnippet <|> escapedChar)) |>> StringLiteral

let pifdef: Parser<_> = pstring "#ifdef" >>. preturn IfDef
let pifndef: Parser<_> = pstring "#ifndef" >>. preturn IfNDef
let pelif: Parser<_> = pstring "#elif" >>. preturn Elif
let pelse: Parser<_> = pstring "#else" >>. preturn Else
let pendif: Parser<_> = pstring "#endif" >>. preturn EndIf
let pif: Parser<_> = pstring "#if" >>. preturn If
let pinclude: Parser<_> = pstring "#include" >>. preturn Include

let tokenParser = many (choice [pcomma; pparenopen; pparenclose; pspace; pdefine; 
                            pidentifier; pstringliteral; pplus; pminus; pdiv; pmul; peq; pneq;
                            pgreater; pless; pgreatereq; plesseq; pintliteral])

let executeTokenParser input = match run tokenParser input with
                               | Success(value, _, _) -> Some value
                               | Failure(_) -> None

let stripWhiteSpace = List.filter (fun x -> x <> Space)