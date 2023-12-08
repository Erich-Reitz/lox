import std/hashes
import std/tables

type
    Token* = object
        typ*: TokenType
        value*: Value
        lexeme*: string
        line*: int

    LoxRuntimeError* = ref object of CatchableError
        token*: Token

    LoxInvalidCast* = object of LoxRuntimeError

    LoxUndefinedVariable* = object of LoxRuntimeError

    LoxUndefinedProperty* = object of LoxRuntimeError


    LoxParseError* = object of CatchableError

    LoxReturn* = object of CatchableError
        value*: Value

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

    FunctionType* = enum
        ftNone,
        ftFunction,
        ftMethod,
        ftInitializer

    ClassType* = enum
        ctNone,
        ctClass,
        ctSubclass

    Resolver* = ref object
        interpreter*: Interpreter
        scopes*: seq[Table[string, bool]]
        curfunction*: FunctionType
        curclass*: ClassType

    Interpreter* = ref object
        globals*: Env
        environment*: Env
        expLocals*: Table[LxExpr, int]

    Env* = ref object of RootObj
        enclosing*: Env
        values*: Table[string, Value]

    LoxCallable* = ref object of RootObj
        arity*: proc (): int
        call*: proc(inter: var Interpreter, args: seq[Value]): Value

    LoxFunction* = ref object of LoxCallable
        declaration*: FuncStmt
        closure*: Env
        isInitializer*: bool

    LoxClass* = ref object of LoxCallable
        name*: string
        methods*: Table[string, LoxFunction]
        superclass*: LoxClass

    LoxInstance* = ref object
        class*: LoxClass
        fields*: Table[string, Value]






    ValueKind* = enum lkBool, lkNum, lkString, lkIden, lkFunction, lkClass, lkInstance

    Value* = ref object of RootObj
        case kind*: ValueKind
        of lkBool: boolVal*: bool
        of lkString, lkIden: strVal*: string
        of lkNum: numVal*: float
        of lkFunction: funcVal*: LoxCallable
        of lkClass: classVal*: LoxClass
        of lkInstance: instanceVal*: LoxInstance


    ExprKind* = enum ekBinary, ekGrouping, ekValue, ekUnary, ekVar, ekAssign,
        ekLogical, ekCall, ekGet, ekSet, ekThis, ekSuper

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

    GetExpr* = object
        obj*: LxExpr
        name*: Token

    SetExpr* = object
        obj*: LxExpr
        name*: Token
        value*: LxExpr

    ThisExpr* = object
        keyword*: Token

    SuperExpr* = object
        keyword*: Token
        methd*: Token

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
        of ekGet: exget*: GetExpr
        of ekSet: exset*: SetExpr
        of ekThis: exthis*: ThisExpr
        of ekSuper: exsuper*: SuperExpr


    StmtKind* = enum skClass, skPrint, skExpr, skVar, skBlock, skIf, skWhile,
        skFunc, skReturn

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

    ReturnStmt* = object
        keyword*: Token
        value*: LxExpr

    ClassStmt* = object
        name*: Token
        # LStmt should all be of type skFunc
        methods*: seq[LStmt]
        # should be of type LxVar
        superclass*: LxExpr

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
        of skReturn:
            returnstmt*: ReturnStmt
        of skClass:
            classstmt*: ClassStmt


proc findMethod*(klass: LoxClass, name: string): LoxFunction =
    if klass.methods.contains(name):
        return klass.methods[name]

    if klass.superclass != nil:
        return findMethod(klass.superclass, name)

    return nil





proc hash(t: LoxCallable): Hash =
    hash(t.arity) xor hash(t.call)

proc hash(t: Value): Hash =
    case t.kind
    of lkBool:
        hash(t.boolVal)
    of lkString, lkIden:
        hash(t.strVal)
    of lkNum:
        hash(t.numVal)
    of lkFunction:
        hash(t.funcVal)
    of lkClass:
        hash(t.classVal.name)
    of lkInstance:
        hash(t.instanceVal.class.name)


proc hash*(x: LxExpr): Hash =
    case x.kind
    of ekBinary:
        hash(x.bin.left) xor hash(x.bin.right)
    of ekGrouping:
        hash(x.group.lexpr)
    of ekValue:
        hash(x.val.val)
    of ekUnary:
        hash(x.unary.right)
    of ekVar:
        hash(x.varex.name)
    of ekAssign:
        hash(x.assign.name)
    of ekLogical:
        hash(x.logical.left) xor hash(x.logical.right)
    of ekCall:
        hash(x.call.callee) xor hash(x.call.paren) xor hash(x.call.args)
    of ekGet:
        hash(x.exget.obj) xor hash(x.exget.name)
    of ekSet:
        hash(x.exset.obj) xor hash(x.exset.name) xor hash(x.exset.value)
    of ekThis:
        hash(x.exthis.keyword)
    of ekSuper:
        hash(x.exsuper.keyword) xor hash(x.exsuper.methd)


