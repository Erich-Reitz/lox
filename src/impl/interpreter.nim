import env
import lexceptions
import lstmt
import lxexpr
import status
import token
import value

# function protoypes
proc evaluate(exp: ValExpr, env: Env): Value
proc evaluate(exp: GroupingExpr, env: Env): Value
proc evaluate(exp: UnaryExpr, env: Env): Value
proc evaluate(exp: BinExpr, env: Env): Value
proc evaluate(exp: VarExpr, env: Env): Value
proc evaluate(exp: AssignExpr, env: Env): Value
proc evaluate(exp: LogicalExpr, env: Env): Value
proc execute(s: LStmt, env: Env)



proc checkNumberOperand(op: Token, operand: Value) =
    if operand.kind == lkNum:
        return

    var exception = newException(LoxInvalidCast, "Operand must be a number")
    exception.token = op
    raise exception

proc checkNumberOperands(op: Token, left: Value, right: Value) =
    if left.kind == lkNum and right.kind == lkNum:
        return

    var exception = newException(LoxInvalidCast, "Operands must be numbers")
    exception.token = op
    raise exception


proc evaluate(exp: LxExpr, env: Env): Value =
    case exp.kind:
        of ekValue:
            evaluate(exp.val, env)
        of ekGrouping:
            evaluate(exp.group, env)
        of ekUnary:
            evaluate(exp.unary, env)
        of ekBinary:
            evaluate(exp.bin, env)
        of ekVar:
            # TODO: check on the compilier error message if this is exp.varexp, matching the type of the field.
            # may be able to improve the error message
            evaluate(exp.varex, env)
        of ekAssign:
            evaluate(exp.assign, env)
        of ekLogical:
            evaluate(exp.logical, env)


proc evaluate(exp: VarExpr, env: Env): Value =
    env.get(exp.name)

proc evaluate(exp: ValExpr, env: Env): Value =
    exp.val

proc evaluate(exp: GroupingExpr, env: Env): Value =
    evaluate(exp.lexpr, env)

proc evaluate(exp: UnaryExpr, env: Env): Value =
    let right = evaluate(exp.right, env)
    case exp.op.typ:
        of tkMinus:
            checkNumberOperand(exp.op, right)
            -right
        of tkBang:
            !right
        else:
            raise newException(Exception, "Invalid unary operator")

proc evaluate(exp: BinExpr, env: Env): Value =
    let left = evaluate(exp.left, env)
    let right = evaluate(exp.right, env)
    case exp.op.typ:
        of tkMinus:
            checkNumberOperands(exp.op, left, right)
            return left - right
        of tkSlash:
            checkNumberOperands(exp.op, left, right)
            return left / right
        of tkStar:
            checkNumberOperands(exp.op, left, right)
            return left * right
        of tkPlus:
            if left.kind == lkNum and right.kind == lkNum:
                return Value(kind: lkNum, numVal: left.numVal + right.numVal)
            elif left.kind == lkString and right.kind == lkString:
                return Value(kind: lkString, strVal: left.strVal & right.strVal)
            else:
                var exception = newException(LoxInvalidCast, "Operands must be two numbers or two strings.")
                exception.token = exp.op
                raise exception
        of tkGreater:
            checkNumberOperands(exp.op, left, right)
            return left > right
        of tkGreaterEqual:
            checkNumberOperands(exp.op, left, right)
            return left >= right
        of tkLess:
            checkNumberOperands(exp.op, left, right)
            return left < right
        of tkLessEqual:
            checkNumberOperands(exp.op, left, right)
            return left <= right
        of tkBangEqual:
            return left != right
        of tkEqualEqual:
            return left == right
        else:
            raise newException(Exception, "Invalid binary operator")


proc evaluate(exp: AssignExpr, env: Env): Value =
    let value = evaluate(exp.value, env)
    env.assign(exp.name, value)
    return value


proc evaluate(exp: LogicalExpr, env: Env): Value =
    let left = evaluate(exp.left, env)
    case exp.op.typ:
        of tkOr:
            if isTruthy(left): return left
        of tkAnd:
            if not isTruthy(left): return left
        else:
            raise newException(Exception, "Invalid logical operator")

    return evaluate(exp.right, env)


proc execute(s: ExprStmt, env: Env) =
    discard evaluate(s.exp, env)

proc execute(s: PrintStmt, env: Env) =
    let v = evaluate(s.exp, env)
    echo v

proc execute(s: VarStmt, env: Env) =
    var value: Value = nil
    if s.init != nil:
        value = evaluate(s.init, env)

    env.define(s.name.lexeme, value)

proc executeBlock(stmts: seq[LStmt], env: Env) =
    for stm in stmts:
        execute(stm, env)


proc execute(s: BlockStmt, env: Env) =
    executeBlock(s.stmts, initEnv(env))

proc execute(s: IfStmt, env: Env) =
    if isTruthy(evaluate(s.cond, env)):
        execute(s.thenBranch, env)
    elif s.elseBranch != nil:
        execute(s.elseBranch, env)


proc execute(s: WhileStmt, env: Env) =
    while isTruthy(evaluate(s.cond, env)):
        execute(s.body, env)

proc execute(s: LStmt, env: Env) =
    case s.kind:
    of skPrint:
        execute(s.print, env)
    of skExpr:
        execute(s.exp, env)
    of skVar:
        execute(s.varstmt, env)
    of skBlock:
        execute(s.blockstmt, env)
    of skIf:
        execute(s.ifstmt, env)
    of skWhile:
        execute(s.whilestmt, env)


proc interpret*(stmts: seq[LStmt]) =
    let env = initEnv(nil)
    try:
        for lstmt in stmts:
            execute(lstmt, env)
    except LoxRuntimeError as e:
        runtimeError(e)

