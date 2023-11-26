import lexceptions
import value
import lxexpr
import status
import lstmt
import token
import tokenType


type Parser* = object
    tokens*: seq[Token]
    current*: int = 0



func peek(p: Parser): Token =
    p.tokens[p.current]

func isAtEnd(p: Parser): bool =
    peek(p).typ == tkEOF

func previous(p: Parser): Token =
    p.tokens[p.current - 1]

func check(p: Parser, typ: TokenType): bool =
    if isAtEnd(p):
        return false

    peek(p).typ == typ

func advance(p: var Parser): Token =
    if isAtEnd(p) == false:
        p.current += 1

    previous(p)

func synchronize(p: var Parser) =
    discard advance(p)

    while isAtEnd(p) == false:
        if previous(p).typ == tkSemicolon:
            return

        case peek(p).typ:
            of tkClass, tkFun, tkVar, tkFor, tkIf, tkWhile, tkPrint, tkReturn:
                return
            else:
                discard

        discard advance(p)


proc consume(p: var Parser, typ: TokenType, msg: string): Token =
    if check(p, typ) == true:
        return advance(p)

    error(peek(p), msg)
    raise newException(LoxParseError, msg)



func match(p: var Parser, typ: TokenType): bool =
    if check(p, typ):
        discard advance(p)
        return true

    return false

proc match*(p: var Parser, types: varargs[TokenType]): bool =
    for typ in types:
        if match(p, typ):
            return true

    return false


proc expression(p: var Parser): LxExpr
proc declaration(p: var Parser): LStmt

proc primary(p: var Parser): LxExpr =
    if match(p, tkFalse):
        return LxExpr(kind: ekValue, val: ValExpr(val: Value(
                kind: lkBool, boolVal: false)))
    elif match(p, tkTrue):
        return LxExpr(kind: ekValue, val: ValExpr(val: Value(
                kind: lkBool, boolVal: true)))
    elif match(p, tkNil):
        return LxExpr(kind: ekValue, val: ValExpr(val: nil))
    elif match(p, tkNumber, tkString):
        return LxExpr(kind: ekValue, val: ValExpr(val: previous(p).value))
    elif match(p, tkIdentifier):
        return LxExpr(kind: ekVar, varex: VarExpr(name: previous(p)))
    elif match(p, tkLeftParen):
        let lexpr = expression(p)
        discard consume(p, tkRightParen, "expect ')' after expression.")
        return LxExpr(kind: ekGrouping, group: GroupingExpr(lexpr: lexpr))
    else:
        error(peek(p), "expect expression.")
        raise newException(LoxParseError, "expect expression.")

proc unary(p: var Parser): LxExpr =
    if match(p, tkBang, tkMinus):
        let op = previous(p)
        let right = unary(p)
        return LxExpr(kind: ekUnary, unary: UnaryExpr(op: op, right: right))

    primary(p)


proc factor(p: var Parser): LxExpr =
    var lexpr = unary(p)

    while match(p, tkSlash, tkStar):
        let op = previous(p)
        let right = unary(p)
        lexpr = LxExpr(kind: ekBinary, bin: BinExpr(left: lexpr, op: op, right: right))

    lexpr

proc term(p: var Parser): LxExpr =
    var lexpr = factor(p)

    while match(p, tkMinus, tkPlus):
        let op = previous(p)
        let right = factor(p)
        lexpr = LxExpr(kind: ekBinary, bin: BinExpr(left: lexpr, op: op, right: right))

    lexpr

proc comparison(p: var Parser): LxExpr =
    var lexpr = term(p)

    while match(p, tkGreater, tkGreaterEqual, tkLess, tkLessEqual):
        let op = previous(p)
        let right = term(p)
        lexpr = LxExpr(kind: ekBinary, bin: BinExpr(left: lexpr, op: op, right: right))

    lexpr

proc equality(p: var Parser): LxExpr =
    var lexpr = comparison(p)

    while match(p, tkBangEqual, tkEqualEqual):
        let op = previous(p)
        let right = comparison(p)
        lexpr = LxExpr(kind: ekBinary, bin: BinExpr(left: lexpr, op: op, right: right))

    lexpr


proc assignment(p: var Parser): LxExpr =
    let lexpr = equality(p)

    if match(p, tkEqual):
        let equals = previous(p)
        let value = assignment(p)

        if lexpr.kind == ekVar:
            let name = lexpr.varex.name
            return LxExpr(kind: ekAssign, assign: AssignExpr(name: name, value: value))

        error(equals, "invalid assignment target.")


    lexpr


proc expression(p: var Parser): LxExpr =
    assignment(p)

proc exprStmt(p: var Parser): LStmt =
    let lexpr = expression(p)
    discard consume(p, tkSemicolon, "expect ';' after expression.")
    return LStmt(kind: skExpr, exp: ExprStmt(exp: lexpr))

proc printStmt(p: var Parser): LStmt =
    let lexpr = expression(p)
    discard consume(p, tkSemicolon, "expect ';' after value.")
    return LStmt(kind: skPrint, print: PrintStmt(exp: lexpr))

proc blockStmt(p: var Parser): LStmt =
    var stmts: seq[LStmt] = @[]

    while isAtEnd(p) == false and check(p, tkRightBrace) == false:
        let stm = declaration(p)
        stmts.add(stm)

    discard consume(p, tkRightBrace, "expect '}' after block.")

    LStmt(kind: skBlock, blockstmt: BlockStmt(stmts: stmts))

proc statement(p: var Parser): LStmt =
    if match(p, tkPrint):
        return printStmt(p)
    if match(p, tkLeftBrace):
        return blockStmt(p)

    return exprStmt(p)


proc varDeclaration(p: var Parser): LStmt =
    let name = consume(p, tkIdentifier, "expect variable name.")

    var initializer: LxExpr = nil
    if match(p, tkEqual):
        initializer = expression(p)

    discard consume(p, tkSemicolon, "expect ';' after variable declaration.")
    return LStmt(kind: skVar, varstmt: VarStmt(name: name, init: initializer))

proc declaration(p: var Parser): LStmt =
    try:
        if match(p, tkVar):
            return varDeclaration(p)
        return statement(p)
    except LoxParseError:
        synchronize(p)
        return nil



proc parse*(tokens: seq[Token]): seq[LStmt] =
    var p = Parser(tokens: tokens)
    var stmts: seq[LStmt] = @[]

    while isAtEnd(p) == false:
        let stm = declaration(p)
        stmts.add(stm)

    stmts
