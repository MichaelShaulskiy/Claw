module Claw.Core.Tokenizer

open System
open FParsec

type ParserState = unit
type Parser<'T> = Parser<'T, ParserState>

type PreprocessorDirective =
    | Define
    | Undef
    | Include
    | IfDef
    | IfNDef
    | IfDirective
    | ElseDirective
    | Elif
    | EndIf
    | Error
    | Pragma
    | Line
    | Warning
    | MixinBegin
    | MixinEnd

type PreprocessorOperator =
    | Stringify          // #
    | TokenPaste         // ##
    | Defined            // defined
    | HasInclude         // __has_include
    | HasAttribute       // __has_attribute

type Literal =
    | IntegerLiteral of int64 * string option    // value * suffix (u, l, ul, etc.)
    | FloatingLiteral of double * string option  // value * suffix (f, l)
    | CharacterLiteral of char * string option   // value * prefix (L, u, U)
    | StringLiteral of string * string option    // value * prefix (L, u8, u, U)
    | BooleanLiteral of bool

type Operator =
    | Plus | Minus | Multiply | Divide | Modulo
    | Increment | Decrement
    
    | Assign | PlusAssign | MinusAssign | MultiplyAssign | DivideAssign | ModuloAssign
    | LeftShiftAssign | RightShiftAssign | BitwiseAndAssign | BitwiseOrAssign | BitwiseXorAssign
    
    | Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual
    
    | LogicalAnd | LogicalOr | LogicalNot
    
    | BitwiseAnd | BitwiseOr | BitwiseXor | BitwiseNot | LeftShift | RightShift
    
    | Conditional | Arrow | Dot | SizeOf | AlignOf | TypeOf
    | ScopeResolution

type Punctuator =
    | LeftParen | RightParen
    | LeftBracket | RightBracket
    | LeftBrace | RightBrace
    | Semicolon | Comma | Colon | Ellipsis
    | Question | Hash | DoubleHash

type Keyword =
    | Auto | Register | Static | Extern | ThreadLocal

    | Void | Char | Short | Int | Long | Float | Double | Signed | Unsigned
    | Bool | Complex | Imaginary
    
    | Const | Restrict | Volatile | Atomic

    | Inline | NoReturn
    | AlignAs
    
    | If | Else | Switch | Case | Default
    | While | For | Do | Break | Continue | Goto | Return
    
    | Struct | Union | Enum | Typedef
    | StaticAssert

type Token =
    | Directive of PreprocessorDirective
    | PreprocessorOperator of PreprocessorOperator
    | Keyword of Keyword
    | Identifier of string
    | Literal of Literal
    | Operator of Operator
    | Punctuator of Punctuator
    | Newline
    | Whitespace of string
    | Comment of string
    | HeaderName of string * bool    // filename * is_system_header
    | MacroParameter of string
    | EOF

let ws = spaces
let ws1 = spaces1

let pidentifier: Parser<Token> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    |>> (fun id -> 
        match id with
        | "auto" -> Keyword Auto
        | "register" -> Keyword Register
        | "static" -> Keyword Static
        | "extern" -> Keyword Extern
        | "_Thread_local" -> Keyword ThreadLocal
        | "void" -> Keyword Void
        | "char" -> Keyword Char
        | "short" -> Keyword Short
        | "int" -> Keyword Int
        | "long" -> Keyword Long
        | "float" -> Keyword Float
        | "double" -> Keyword Double
        | "signed" -> Keyword Signed
        | "unsigned" -> Keyword Unsigned
        | "_Bool" -> Keyword Bool
        | "_Complex" -> Keyword Complex
        | "_Imaginary" -> Keyword Imaginary
        | "const" -> Keyword Const
        | "restrict" -> Keyword Restrict
        | "volatile" -> Keyword Volatile
        | "_Atomic" -> Keyword Atomic
        | "inline" -> Keyword Inline
        | "_Noreturn" -> Keyword NoReturn
        | "_Alignas" -> Keyword AlignAs
        | "if" -> Keyword If
        | "else" -> Keyword Else
        | "switch" -> Keyword Switch
        | "case" -> Keyword Case
        | "default" -> Keyword Default
        | "while" -> Keyword While
        | "for" -> Keyword For
        | "do" -> Keyword Do
        | "break" -> Keyword Break
        | "continue" -> Keyword Continue
        | "goto" -> Keyword Goto
        | "return" -> Keyword Return
        | "struct" -> Keyword Struct
        | "union" -> Keyword Union
        | "enum" -> Keyword Enum
        | "typedef" -> Keyword Typedef
        | "_Static_assert" -> Keyword StaticAssert
        | "sizeof" -> Operator SizeOf
        | "_Alignof" -> Operator AlignOf
        | "typeof" -> Operator TypeOf
        | "defined" -> PreprocessorOperator Defined
        | "__has_include" -> PreprocessorOperator HasInclude
        | "__has_attribute" -> PreprocessorOperator HasAttribute
        | _ -> Identifier id)

let pintegerLiteral: Parser<Token> =
    let suffix = 
        choice [
            pstringCI "ull" <|> pstringCI "llu"
            pstringCI "ul" <|> pstringCI "lu"
            pchar 'u' <|> pchar 'U' |>> string
            pchar 'l' <|> pchar 'L' |>> string
            pstringCI "ll"
        ]
    
    let hexNumber = pstring "0x" <|> pstring "0X" >>. many1 hex |>> (fun digits -> digits |> List.map string |> String.concat "")
    let octNumber = pchar '0' >>. many (anyOf "01234567") |>> (fun digits -> "0" + (digits |> List.map string |> String.concat ""))
    let decNumber = many1 digit |>> (fun digits -> digits |> List.map string |> String.concat "")
    
    (hexNumber <|> octNumber <|> decNumber)
    .>>. opt suffix
    |>> (fun (numStr, suf) -> 
        let value = System.Convert.ToInt64(numStr, if numStr.StartsWith("0x") || numStr.StartsWith("0X") then 16 elif numStr.StartsWith("0") && numStr.Length > 1 then 8 else 10)
        Literal (IntegerLiteral (value, suf)))

let pfloatingLiteral: Parser<Token> =
    let suffix = choice [pchar 'f'; pchar 'F'; pchar 'l'; pchar 'L'] |>> string
    
    // Simple floating point: digits.digits or digits.digitsE+/-digits
    let decimalNumber = 
        many1 digit .>>. (pchar '.' >>. many digit) .>>. opt ((pchar 'e' <|> pchar 'E') >>. opt (pchar '+' <|> pchar '-') .>>. many1 digit)
        |>> (fun ((whole, frac), exp) ->
            let wholeStr = whole |> List.map string |> String.concat ""
            let fracStr = frac |> List.map string |> String.concat ""
            let expStr = match exp with 
                         | Some (sign, digits) -> 
                             let signStr = match sign with Some s -> string s | None -> ""
                             let digitsStr = digits |> List.map string |> String.concat ""
                             "e" + signStr + digitsStr
                         | None -> ""
            wholeStr + "." + fracStr + expStr)
    
    decimalNumber .>>. opt suffix
    |>> (fun (numStr, suf) -> Literal (FloatingLiteral (Double.Parse(numStr), suf)))

let pcharacterLiteral: Parser<Token> =
    let prefix = choice [pchar 'L'; pchar 'u'; pchar 'U'] |>> string
    let escapedChar = 
        pchar '\\' >>. choice [
            pchar 'n' >>% '\n'
            pchar 'r' >>% '\r'
            pchar 't' >>% '\t'
            pchar '\\' >>% '\\'
            pchar '\'' >>% '\''
            pchar '"' >>% '"'
            pchar '0' >>% '\000'
            anyChar
        ]
    let regularChar = satisfy (fun c -> c <> '\'' && c <> '\\')
    
    opt prefix .>>. between (pchar '\'') (pchar '\'') (escapedChar <|> regularChar)
    |>> (fun (pre, c) -> Literal (CharacterLiteral (c, pre)))

let pstringLiteral: Parser<Token> =
    let prefix = choice [pstring "u8"; pchar 'u' |>> string; pchar 'U' |>> string; pchar 'L' |>> string]
    let escapedChar = 
        pchar '\\' >>. choice [
            pchar 'n' >>% "\n"
            pchar 'r' >>% "\r"
            pchar 't' >>% "\t"
            pchar '\\' >>% "\\"
            pchar '"' >>% "\""
            pchar '\'' >>% "'"
            pchar '0' >>% "\000"
            anyChar |>> string
        ]
    let regularChars = many1Satisfy (fun c -> c <> '"' && c <> '\\')
    
    opt prefix .>>. between (pchar '"') (pchar '"') (manyStrings (regularChars <|> escapedChar))
    |>> (fun (pre, s) -> Literal (StringLiteral (s, pre)))

let pheaderName: Parser<Token> =
    let systemHeader = between (pchar '<') (pchar '>') (many1Satisfy (fun c -> c <> '>'))
    let localHeader = between (pchar '"') (pchar '"') (many1Satisfy (fun c -> c <> '"'))
    
    (systemHeader |>> (fun h -> HeaderName (h, true))) <|>
    (localHeader |>> (fun h -> HeaderName (h, false)))

let poperators: Parser<Token> =
    choice [
        pstring "<<=" >>% Operator LeftShiftAssign
        pstring ">>=" >>% Operator RightShiftAssign
        pstring "++" >>% Operator Increment
        pstring "--" >>% Operator Decrement
        pstring "+=" >>% Operator PlusAssign
        pstring "-=" >>% Operator MinusAssign
        pstring "*=" >>% Operator MultiplyAssign
        pstring "/=" >>% Operator DivideAssign
        pstring "%=" >>% Operator ModuloAssign
        pstring "&=" >>% Operator BitwiseAndAssign
        pstring "|=" >>% Operator BitwiseOrAssign
        pstring "^=" >>% Operator BitwiseXorAssign
        pstring "==" >>% Operator Equal
        pstring "!=" >>% Operator NotEqual
        pstring "<=" >>% Operator LessEqual
        pstring ">=" >>% Operator GreaterEqual
        pstring "&&" >>% Operator LogicalAnd
        pstring "||" >>% Operator LogicalOr
        pstring "<<" >>% Operator LeftShift
        pstring ">>" >>% Operator RightShift
        pstring "->" >>% Operator Arrow
        pstring "::" >>% Operator ScopeResolution
        pstring "..." >>% Punctuator Ellipsis
        pstring "##" >>% Punctuator DoubleHash
        
        pchar '+' >>% Operator Plus
        pchar '-' >>% Operator Minus
        pchar '*' >>% Operator Multiply
        pchar '/' >>% Operator Divide
        pchar '%' >>% Operator Modulo
        pchar '=' >>% Operator Assign
        pchar '<' >>% Operator Less
        pchar '>' >>% Operator Greater
        pchar '!' >>% Operator LogicalNot
        pchar '&' >>% Operator BitwiseAnd
        pchar '|' >>% Operator BitwiseOr
        pchar '^' >>% Operator BitwiseXor
        pchar '~' >>% Operator BitwiseNot
        pchar '?' >>% Operator Conditional
        pchar '.' >>% Operator Dot
    ]

let ppunctuators: Parser<Token> =
    choice [
        pchar '(' >>% Punctuator LeftParen
        pchar ')' >>% Punctuator RightParen
        pchar '[' >>% Punctuator LeftBracket
        pchar ']' >>% Punctuator RightBracket
        pchar '{' >>% Punctuator LeftBrace
        pchar '}' >>% Punctuator RightBrace
        pchar ';' >>% Punctuator Semicolon
        pchar ',' >>% Punctuator Comma
        pchar ':' >>% Punctuator Colon
        pchar '#' >>% Punctuator Hash
    ]

let pmixinDirective: Parser<Token> = ws >>. choice [
    pstring "@/*" >>% Directive MixinBegin
    pstring "*/@" >>% Directive MixinEnd
    ]

let ppreprocessorDirective: Parser<Token> =
    let pcpreprocessordirective = 
        pchar '#' >>. ws >>. choice [
            pstring "define" >>% Directive Define
            pstring "undef" >>% Directive Undef
            pstring "include" >>% Directive Include
            pstring "ifdef" >>% Directive IfDef
            pstring "ifndef" >>% Directive IfNDef
            pstring "if" >>% Directive IfDirective
            pstring "else" >>% Directive ElseDirective
            pstring "elif" >>% Directive Elif
            pstring "endif" >>% Directive EndIf
            pstring "error" >>% Directive Error
            pstring "pragma" >>% Directive Pragma
            pstring "line" >>% Directive Line
            pstring "warning" >>% Directive Warning
        ]
    choice [pcpreprocessordirective; pmixinDirective]

let pcomment: Parser<Token> =
    let singleLineComment = 
        pstring "//" >>. restOfLine false |>> (fun content -> Comment ("// " + content))
    let multiLineComment = 
        between (pstring "/*") (pstring "*/") (manyCharsTill anyChar (pstring "*/"))
        |>> (fun content -> Comment ("/*" + content + "*/"))
    
    singleLineComment <|> multiLineComment

let pwhitespace: Parser<Token> =
    many1 (satisfy (fun c -> c = ' ' || c = '\t' || c = '\r')) |>> (String.Concat >> Whitespace)

let pnewline: Parser<Token> =
    pchar '\n' >>% Newline

let ptoken: Parser<Token> =
    choice [
        pcomment
        ppreprocessorDirective
        pfloatingLiteral
        pintegerLiteral
        pcharacterLiteral
        pstringLiteral
        pheaderName
        pidentifier
        poperators
        ppunctuators
        pwhitespace
        pnewline
    ]

let tokenize input: Token list =
    match run (many ptoken .>> eof) input with
    | Success (tokens, _, _) -> tokens @ [EOF]
    | Failure (errorMsg, _, _) -> 
        // Fallback: try to parse character by character
        let rec parseChars acc remaining =
            if String.length remaining = 0 then
                List.rev (EOF :: acc)
            else
                match run ptoken remaining with
                | Success (token, _, pos) when pos.Index > 0L ->
                    let newRemaining = remaining.Substring(int pos.Index)
                    parseChars (token :: acc) newRemaining
                | _ ->
                    // Skip problematic character
                    parseChars acc (remaining.Substring(1))
        parseChars [] input

let tokenizeFile filePath =
    System.IO.File.ReadAllText(filePath) |> tokenize

let filterWhitespace tokens =
    tokens |> List.filter (function 
        | Whitespace _ -> false 
        | _ -> true)

let filterComments tokens =
    tokens |> List.filter (function 
        | Comment _ -> false 
        | _ -> true)

let getDirectives tokens =
    tokens |> List.choose (function 
        | Directive d -> Some d 
        | _ -> None)

let getIdentifiers tokens =
    tokens |> List.choose (function 
        | Identifier id -> Some id 
        | _ -> None)

let tokenToString = function
    | Directive d -> sprintf "#%A" d
    | PreprocessorOperator op -> sprintf "%A" op
    | Keyword kw -> sprintf "%A" kw
    | Identifier id -> sprintf "ID(%s)" id
    | Literal lit -> sprintf "%A" lit
    | Operator op -> sprintf "%A" op
    | Punctuator p -> sprintf "%A" p
    | Newline -> "\\n"
    | Whitespace ws -> sprintf "WS(%s)" (ws.Replace(" ", "·").Replace("\t", "→"))
    | Comment c -> sprintf "COMMENT(%s)" c
    | HeaderName (name, isSystem) -> sprintf "HEADER(%s,%b)" name isSystem
    | MacroParameter p -> sprintf "MACRO_PARAM(%s)" p
    | EOF -> "EOF"

let printTokens tokens =
    tokens |> List.iter (tokenToString >> printfn "%s")

let stripWhiteSpace = List.filter (function 
    | Whitespace _ -> false 
    | _ -> true)