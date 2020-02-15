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
    | TypeOf
    | SquareBracketOpen
    | SquareBracketClose
    | Percent
    | ParenOpen
    | ParenClose
    | Comma
    | Asterisk
    | Ampersand
    | Assignment
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
    | Underscore
    | Dot
    | Modulo
    | Increment
    | Decrement
    | IncrementAsign
    | DecrementAsign
    | ShiftLeft
    | ShiftLeftAsign
    | ShiftRight
    | ShiftRightAsign
    | XOr
    | MulAsign
    | DivAsign
    | ModuloAsign
    | LogicalAnd
    | LogicalOr
    | LogicalNot
    | Complement
    | BitwiseAndAsing
    | BitwiseXOrAsign
    | BitwiseORAsign
    | Arrow
    | TernaryQuestionMark
    | Signed
    | ScopeOp
    | SingleLineComment
    | MultiLineCommentStart
    | MultiLineCommentStop
    | BatchShift
    | AtSign
    | BackTick
    | Asm
    | Auto
    | Bool
    | Break
    | Case
    | Catch
    | Char
    | Class
    | Const
    | ConstExpr
    | Continue
    | DeclType
    | Default
    | Delete
    | Do
    | Double
    | Else
    | Enum
    | Explicit
    | Export
    | Extern
    | BoolLiteral of bool
    | Float
    | FloatLiteral of double
    | Goto
    | IfStatement
    | Inline
    | Int
    | Long
    | Mutable
    | Namespace
    | New
    | NoExcept
    | NullPtr
    | Private
    | Protected
    | Public
    | Register
    | Return
    | Short
    | Static
    | Struct
    | Switch
    | Template
    | This
    | Throw
    | Try
    | TypeDef
    | TypeId
    | TypeName
    | Union
    | Using
    | Virtual
    | Void
    | Volatile
    | While
    | StaticCast
    | ConstCast
    | ReinterpretCast

let stringToken s tret: Parser<_> = pstring s >>. preturn tret

let (<!>) (p: Parser<_, _>) label: Parser<_, _> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leving %s (%A)" stream.Position label reply.Status
        reply

// constructs a lifted pchar parser
let liftPChar symbol output: Parser<_> = pchar symbol >>. preturn output
let liftPString s output: Parser<_> = pstring s >>. preturn output


let genParser (tup: Token * string): Parser<_> =
    match tup with
    | (T, xs) when String.length xs > 1 -> pstring xs >>. preturn T
    | (T, xs) -> pchar (char xs) >>. preturn T

let parsers =
    [ 
      (Hash, "#")
      (AtSign, "@")
      (Semicolon, ";")
      (Define, "#define")
      (Space, " ") 
      (Unsigned, "unsigned")
      (Signed, "signed")
      (SizeOf, "sizeof")
      (TypeOf, "typeof")
      (SquareBracketOpen, "[")
      (SquareBracketClose, "]")
      (ModuloAsign, "%=")
      (Percent, "%")
      (ParenOpen, "(")
      (ParenClose, ")")
      (Comma, ",")
      (Asterisk, "*")
      (LogicalAnd, "&&")
      (Ampersand, "&")
      (CurlyBraceOpen, "{")
      (CurlyBraceClose, "}")
      (MultiLineCommentStart, @"/*")
      (MultiLineCommentStop, @"*/")
      (SingleLineComment, @"//")
      (Increment, "++")
      (Decrement, "--")
      (Plus, "+")
      (Arrow, "->")
      (Minus, "-")
      (Div, "/")
      (Eq, "==")
      (IncrementAsign, "+=")
      (DecrementAsign, "-=")
      (ShiftLeftAsign, "<<=")
      (ShiftRightAsign, ">>=")
      (BitwiseAndAsing, "&=")
      (BitwiseXOrAsign, "^=")
      (BitwiseORAsign, "|=")
      (MulAsign, "*=")
      (DivAsign, "/=")
      (Assignment, "=")
      (NEq, "!=")
      (GreaterEq, ">=")
      (LessEq, "<=")
      (ShiftLeft, "<<")
      (ShiftRight, ">>")
      (Greater, ">")
      (Less, "<")
      (IfDef, "#ifdef")
      (IfNDef, "#ifndef")
      (Elif, "#elif")
      (EndIf, "#endif")
      (TError, "#error")
      (Pragma, "#pragma")
      (Include, "#include")
      (Underscore, "_")
      (LogicalNot, "!")
      (LogicalOr, "||")
      (Complement, "~")
      (QuestionMark, "?")
      (ScopeOp, "::")
      (Colon, ":")
      (VarArgs, "...")
      (Dot, ".")
      (BatchShift, "shift")
      (BackTick, "``")
      ]

let generatedParsers = List.map genParser parsers

let pidentifier: Parser<_> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    (many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier") |>> Identifier

let pintliteral: Parser<_> = pint64 |>> IntLiteral

let pstringliteral =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')

    let escapedChar =
        pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                          | 'n' -> "\n"
                          | 'r' -> "\r"
                          | 't' -> "\t"
                          | c -> string c)
    between (pstring "\"") (pstring "\"") (manyStrings (normalCharSnippet <|> escapedChar)) |>> StringLiteral


let tokenParser = many (choice (List.rev (List.append generatedParsers [pidentifier; pintliteral; pstringliteral])))
let executeTokenParser input =
    match run tokenParser input with
    | Success(value, _, _) -> Some value
    | Failure(_) -> None

let unpackMaybeParseResult (xs: list<Token> option) =
    match xs with
    | Some x -> x
    | _ -> [ NOP ]


let stripWhiteSpace = List.filter (fun x -> x <> Space)
