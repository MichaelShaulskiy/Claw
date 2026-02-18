namespace Claw.Core

module TokenizerTypes =
    open FParsec
    type IndentationState = {
        IndentStack: int list
        CurrentIndent: int
    }

    type ParserState = {
        Indentation: IndentationState
    }

    // type ParserState = unit
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
        | ScopeResolution | TypeSignatureOp

    type Punctuator =
        | LeftParen | RightParen
        | LeftBracket | RightBracket
        | LeftBrace | RightBrace
        | Semicolon | Comma | Colon | Ellipsis
        | Question | Hash | DoubleHash | SingleArrowRight
        | SingleArrowLeft
        | Underscore | Dollar | Backtick
        | Dot| Star | DoubleColon | Pipe

    type Keyword =

        | Void | Int | Long | Float | Double
        | Bool | Char
        
        | Const | Restrict | Volatile | Atomic

        | Inline | NoReturn
        | AlignAs
        
        | Match | With | In | Else | Switch | Case | Default
        | While | For | Do | Break | Continue | Goto | Return
        
        | Struct | Union | Enum | Typedef
        | StaticAssert
        | Namespace | Module | Let | Where 
        | Open
        | Try | Catch | Finally
        | Mut | Rec


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
        | Indent
        | Dedent
        | EOF

