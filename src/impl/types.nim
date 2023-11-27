import std/tables
type

    LoxRuntimeError* = ref object of CatchableError
        token*: Token

    LoxInvalidCast* = object of LoxRuntimeError

    LoxUndefinedVariable* = object of LoxRuntimeError

    LoxParseError* = object of CatchableError


    TokenType* = enum
        tkLeftParen, tkRightParen, tkLeftBrace, tkRightBrace,
        tkComma, tkDot, tkMinus, tkPlus, tkSemicolon, tkSlash, tkStar,

        # One or two character tokens.
        tkBang, tkBangEqual,
        tkEqual, tkEqualEqual,
        tkGreater, tkGreaterEqual,
        tkLess, tkLessEqual,

        # Literals.
        tkIdentifier, tkString, tkNumber,

        # Keywords.
        tkAnd, tkClass, tkElse, tkFalse, tkFun, tkFor, tkIf, tkNil, tkOr,
        tkPrint, tkReturn, tkSuper, tkThis, tkTrue, tkVar, tkWhile,

        tkEOF


    Token* = object
        typ*: TokenType
        value*: Value
        lexeme*: string
        line*: int

    Interpreter* = object
        globals*: Env
        environment*: Env

    Env* = ref object of RootObj
        enclosing*: Env
        values*: Table[string, Value]

    LoxCallable* = ref object of RootObj
        arity*: int
        call*: proc(inter: var Interpreter, args: seq[Value]): Value

    LoxFunction* = ref object of LoxCallable
        declaration*: FuncStmt



    ValueKind* = enum lkBool, lkNum, lkString, lkIden, lkFunction

    Value* = ref object of RootObj
        case kind*: ValueKind
        of lkBool: boolVal*: bool
        of lkString, lkIden: strVal*: string
        of lkNum: numVal*: float
        of lkFunction: funcVal*: LoxCallable


    ExprKind* = enum ekBinary, ekGrouping, ekValue, ekUnary, ekVar, ekAssign,
        ekLogical, ekCall

    BinExpr* = object
        left*: LxExpr
        op*: Token
        right*: LxExpr

    GroupingExpr* = object
        lexpr*: LxExpr

    ValExpr* = object
        val*: Value

    UnaryExpr* = object
        op*: Token
        right*: LxExpr

    VarExpr* = object
        name*: Token

    AssignExpr* = object
        name*: Token
        value*: LxExpr

    LogicalExpr* = object
        left*: LxExpr
        op*: Token
        right*: LxExpr

    CallExpr* = object
        callee*: LxExpr
        paren*: Token
        args*: seq[LxExpr]

    LxExpr* = ref object
        case kind*: ExprKind
        of ekBinary: bin*: BinExpr
        of ekGrouping: group*: GroupingExpr
        of ekValue: val*: ValExpr
        of ekUnary: unary*: UnaryExpr
        of ekVar: varex*: VarExpr
        of ekAssign: assign*: AssignExpr
        of ekLogical: logical*: LogicalExpr
        of ekCall: call*: CallExpr


    StmtKind* = enum skPrint, skExpr, skVar, skBlock, skIf, skWhile, skFunc

    ExprStmt* = object
        exp*: LxExpr

    PrintStmt* = object
        exp*: LxExpr

    VarStmt* = object
        name*: Token
        init*: LxExpr

    BlockStmt* = object
        stmts*: seq[LStmt]

    IfStmt* = object
        cond*: LxExpr
        thenBranch*: LStmt
        elseBranch*: LStmt

    WhileStmt* = object
        cond*: LxExpr
        body*: LStmt

    FuncStmt* = object
        name*: Token
        params*: seq[Token]
        body*: seq[LStmt]

    LStmt* = ref object
        case kind*: StmtKind
        of skPrint:
            print*: PrintStmt
        of skExpr:
            exp*: ExprStmt
        of skVar:
            varstmt*: VarStmt
        of skBlock:
            blockstmt*: BlockStmt
        of skIf:
            ifstmt*: IfStmt
        of skWhile:
            whilestmt*: WhileStmt
        of skFunc:
            funcstmt*: FuncStmt
