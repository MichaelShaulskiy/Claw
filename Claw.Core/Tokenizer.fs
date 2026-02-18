namespace Claw.Core
module Tokenizer = 
    open System
    open FParsec
    open Claw.Core.TokenizerTypes


    let getIndentLevel (state: IndentationState) = state.CurrentIndent

    let pushIndent level (state: IndentationState) =
        { state with IndentStack = level :: state.IndentStack; CurrentIndent = level }

    let popIndent (state: IndentationState) = match state.IndentStack with
                                                | [] -> state
                                                | _::xs -> { state with IndentStack = xs; CurrentIndent = List.tryHead xs |> Option.defaultValue 0}

    let initialIndentState = {
        IndentStack = [0]
        CurrentIndent = 0
    }

    let initParserState = {
        Indentation = initialIndentState
    }

    let pindentation: Parser<int> = 
        many (pchar ' ' <|> pchar '\t')
        |>> fun chars ->
            chars |> List.fold (fun acc c ->
                match c with
                | ' ' -> acc + 1
                | '\t' -> acc + 4

                | _ -> acc) 0

    let plinewithIndent plineContent: Parser<int * 'a> =
        parse {
            let! indent = pindentation
            let! content = plineContent
            return (indent, content)
        }

    let processIndentation (oldLevel: int) (newLevel: int) : Token list =
        if newLevel > oldLevel then
            [Indent]
        elif newLevel < oldLevel then
            // Mehrere DEDENTs möglich
            let dedentCount = (oldLevel - newLevel) / 4  // oder deine Tab-Größe
            List.replicate dedentCount Dedent
        else
            []



    // Alternative: Stateful Parser mit getUserState/setUserState
    let pindentationStateful: Parser<Token list> =
        parse {
            let! state = getUserState
            let! newIndent = pindentation
            let oldIndent = state.Indentation.CurrentIndent
            
            let indentTokens = processIndentation oldIndent newIndent
            
            if newIndent > oldIndent then
                do! updateUserState (fun s -> { s with Indentation = pushIndent newIndent s.Indentation })
                return indentTokens
            elif newIndent < oldIndent then
                do! updateUserState (fun s -> { s with Indentation = popIndent s.Indentation })
                return indentTokens
            else
                return indentTokens
        }


    let ws = spaces
    let ws1 = spaces1

    let pidentifier: Parser<Token> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        |>> fun id -> 
            match id with
            | "namespace" -> Keyword Namespace
            | "module" -> Keyword Module
            | "open" -> Keyword Open
            | "let" -> Keyword Let
            | "where" -> Keyword Where
            | "void" -> Keyword Void
            | "char" -> Keyword Char
            | "int" -> Keyword Int
            | "long" -> Keyword Long
            | "float" -> Keyword Float
            | "double" -> Keyword Double
            | "bool" -> Keyword Bool
            | "mut" -> Keyword Mut
            | _ -> Identifier id

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
            |>> fun ((whole, frac), exp) ->
            let wholeStr = whole |> List.map string |> String.concat ""
            let fracStr = frac |> List.map string |> String.concat ""
            let expStr = 
                match exp with 
                | Some (sign, digits) -> 
                    let signStr = match sign with Some s -> string s | None -> ""
                    let digitsStr = digits |> List.map string |> String.concat ""
                    "e" + signStr + digitsStr
                    | None -> ""
            wholeStr + "." + fracStr + expStr
        
        decimalNumber .>>. opt suffix
        |>> fun (numStr, suf) -> Literal (FloatingLiteral (Double.Parse(numStr), suf))

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
        |>> fun (pre, c) -> Literal (CharacterLiteral (c, pre))

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
        |>> fun (pre, s) -> Literal (StringLiteral (s, pre))

    let pheaderName: Parser<Token> =
        let systemHeader = between (pchar '<') (pchar '>') (many1Satisfy (fun c -> c <> '>'))
        let localHeader = between (pchar '"') (pchar '"') (many1Satisfy (fun c -> c <> '"'))
        
        (systemHeader |>> (fun h -> HeaderName (h, true))) <|>
        (localHeader |>> (fun h -> HeaderName (h, false)))

    let ppath: Parser<Token> = 
        let ppathliteral = many1Satisfy (fun c -> c <> '"' && c <> '\\')

        (ppathliteral .>>. pchar '.') |>> (fun (a, b) -> Identifier $"{a}{b}")
        <|>
        (ppathliteral |>> (fun p -> Identifier p))

    let poperators: Parser<Token> =
        let stringOps = [
            ("<<=", Operator LeftShiftAssign)
            (">>=", Operator RightShiftAssign)
            ("++",Operator Increment)
            ("--",Operator Decrement)
            ("+=",Operator PlusAssign)
            ("-=",Operator MinusAssign)
            ("*=",Operator MultiplyAssign)
            ("/=",Operator DivideAssign)
            ("%=",Operator ModuloAssign)
            ("&=",Operator BitwiseAndAssign)
            ("|=",Operator BitwiseOrAssign)
            ("^=",Operator BitwiseXorAssign)
            ("==",Operator Equal)
            ("!=",Operator NotEqual)
            ("<=",Operator LessEqual)
            (">=",Operator GreaterEqual)
            ("&&",Operator LogicalAnd)
            ("||",Operator LogicalOr)
            ("<<",Operator LeftShift)
            (">>",Operator RightShift)
            ("...", Punctuator Ellipsis)
            ("##",Punctuator DoubleHash)

        ]
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
            pchar '.' >>% Punctuator Dot
        ]

    let ppunctuators: Parser<Token> =
        choice [
            pstring "->" >>% Punctuator SingleArrowRight
            pstring "<-" >>% Punctuator SingleArrowLeft
            pstring "::" >>% Punctuator DoubleColon
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
            pchar '*' >>% Punctuator Star
            pchar '`' >>% Punctuator Backtick
            pchar '_' >>% Punctuator Underscore
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
            pnewline
            pwhitespace
            ppreprocessorDirective
            pidentifier
            poperators
            ppunctuators
            pintegerLiteral
            pfloatingLiteral
        ]

    let tokenize input: Token list =
        match runParserOnString (many ptoken .>> eof) initParserState "" input with
        | Success (tokens, _, _) -> tokens @ [EOF]
        | Failure (errorMsg, _, _) -> 
            // Fallback
            [EOF]

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
        | Indent -> "INDENT"
        | Dedent -> "DEDENT"
        | EOF -> "EOF"

    let printTokens tokens =
        tokens |> List.iter (tokenToString >> printfn "%s")

    let stripWhiteSpace = List.filter (function 
        | Whitespace _ -> false 
        | _ -> true)

    let tokenizeWithIndentation (input: string): Token list =
        let lines = input.Split([|'\n'|])
        let mutable currentIndent = 0
        let mutable tokens = []
        
        // Parser für restliche Zeile (einfache Version)
        let plineTokens = many (choice [pcomment; pwhitespace; pidentifier; poperators; ppunctuators])
        
        for line in lines do
            if String.IsNullOrWhiteSpace(line) then
                tokens <- tokens @ [Newline]
            else
                match runParserOnString pindentation initParserState "" line with
                | Success (indent, _, _) ->
                    let indentTokens = processIndentation currentIndent indent
                    tokens <- tokens @ indentTokens
                    currentIndent <- indent
                    
                    let restOfLine = line.TrimStart()
                    match runParserOnString plineTokens initParserState "" restOfLine with
                    | Success (lineTokens, _, _) ->
                        tokens <- tokens @ lineTokens @ [Newline]
                    | Failure _ ->
                        tokens <- tokens @ [Newline]
                | Failure _ ->
                    tokens <- tokens @ [Newline]
        
        // Dedents am Ende für alle offenen Indentation-Levels
        let finalDedents = List.replicate (currentIndent / 4) Dedent
        tokens @ finalDedents @ [EOF]

