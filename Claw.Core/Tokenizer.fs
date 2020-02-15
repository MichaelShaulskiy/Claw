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
    | Underscore
    | Dot
    | Modulo
    | Increment
    | Decrement
    | IncrementEq
    | DecrementEq
    | ShiftLeft
    | ShiftRight
    | XOr
    | PlusEq
    | MinusEq
    | MulEq
    | DivEq
    | ModuloEq
    | LogicalAnd
    | LogicalOr
    | LogicalNot
    | Complement
    | BitwiseAndEq
    | BitwiseXOrEq
    | ShiftLeftEq
    | ShiftRightEq
    | Arrow
    | TernaryQuestionMark
    | Signed

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

let parsers = [(Hash, "#"); (Semicolon, ";")]


let str s = pstring s
let phash = Hash |> liftPChar '#'
let psemicolon = Semicolon |> liftPChar ';'
let punsigned = Unsigned |> liftPString "unsigned"
let psigned = Signed |> liftPString "signed"
let psizeof = SizeOf |> liftPString "sizeof"
let psquarebracketopen = SquareBracketOpen |> liftPChar '['
let psquarebracketclose = SquareBracketClose |> liftPChar ']'
let pcurlybraceopen = CurlyBraceOpen |> liftPChar '{'
let pcurlybraceClose = CurlyBraceClose |> liftPChar '}'
let pnewline = LineBreak |> liftPChar '\n'
let pand = And |> liftPChar '&'
let plogicand = LogicalAnd |> liftPString "&&"
let por = Or |> liftPChar '|'
let plogicor = LogicalOr |> liftPString "||"
let pnot = Not |> liftPChar '~'
let plogicnot = LogicalNot |> liftPChar '!'
let pquestion = QuestionMark |> liftPChar '?'
let pcolon = Colon |> liftPChar ':'
let punderscore = Underscore |> liftPChar '_'
let pdot = Dot |> liftPChar '.'
let pincrement = Increment |> liftPString "++"
let pdecrement = Decrement |> liftPString "--"
let pincrementeq = IncrementEq |> liftPString "+="
let pdecrementeq = DecrementEq |> liftPString "-="
let pshiftleft = ShiftLeft |> liftPString "<<"
let pshiftright = ShiftRight |> liftPString ">>"
let pxor = XOr |> liftPChar '^'
let pmuleq = MulEq |> liftPString "*="
let pdiveq = DivEq |> liftPString "/="
let pmoduloeq = ModuloEq |> liftPString "%="
let parrow = Arrow |> liftPString "->"
let ppercent = Percent |> liftPChar '%'
let pcomma = Comma |> liftPChar ','
let pplus = Plus |> liftPChar '+'
let pminus = Minus |> liftPChar '-'
let pdiv = Div |> liftPChar '/'
let pmul = Mul |> liftPChar '*'
let peq = Eq |> liftPString "=="
let pneq = NEq |> liftPString "!="
let pgreater = Greater |> liftPChar '>'
let pless = Less |> liftPChar '<'
let pgreatereq = GreaterEq |> liftPString ">="
let plesseq = LessEq |> liftPString "<="
let pparenopen = ParenOpen |> liftPChar '('
let pparenclose = ParenClose |> liftPChar ')'
let pspace = Space |> liftPChar ' '
let pvarargs = VarArgs |> liftPString "..."
let pdefine = Define |> liftPString "#define"

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

let pifdef = IfDef |> liftPString "#ifdef"
let pifndef = IfNDef |> liftPString "#ifndef"
let pelif = Elif |> liftPString "#elif"
let pelse = Else |> liftPString "#else"
let pendif = EndIf |> liftPString "#endif"
let pif = If |> liftPString "#if"
let pinclude = Include |> liftPString "#include"

let tokenParser =
    many
        (choice
            [ pcomma
              psemicolon
              punsigned
              psigned
              psizeof
              psquarebracketopen
              psquarebracketclose
              pcurlybraceopen
              pcurlybraceClose
              pnewline
              plogicand
              plogicnot
              plogicor
              pnot
              por
              pxor
              pquestion
              pcolon
              punderscore
              pvarargs
              pdot
              pincrement
              pincrementeq
              pdecrement
              pdecrementeq
              pshiftleft
              pshiftright
              pmuleq
              pdiveq
              pmoduloeq
              parrow
              ppercent
              pand
              pplus
              pminus
              pdiv
              pmul
              peq
              pneq
              pgreatereq
              plesseq
              pgreater
              pless
              pparenopen
              pparenclose
              pspace
              pdefine
              pidentifier
              pintliteral
              pstringliteral
              pifdef
              pifndef
              pelif
              pelse
              pendif
              pif
              pinclude
              phash ])

let executeTokenParser input =
    match run tokenParser input with
    | Success(value, _, _) -> Some value
    | Failure(_) -> None

let unpackMaybeParseResult (xs: list<Token> option) = match xs with
                                                      | Some x -> x
                                                      | _ -> [NOP]


let stripWhiteSpace = List.filter (fun x -> x <> Space)
