import builtins
import env
import status
import value
import types

# function protoypes
proc evaluate(exp: ValExpr, inter: var Interpreter): Value
proc evaluate(exp: GroupingExpr, inter: var Interpreter): Value
proc evaluate(exp: UnaryExpr, inter: var Interpreter): Value
proc evaluate(exp: BinExpr, inter: var Interpreter): Value
proc evaluate(exp: VarExpr, inter: var Interpreter): Value
proc evaluate(exp: AssignExpr, inter: var Interpreter): Value
proc evaluate(exp: LogicalExpr, inter: var Interpreter): Value
proc evaluate(exp: CallExpr, inter: var Interpreter): Value
proc execute(s: LStmt, inter: var Interpreter)
proc executeBlock*(stmts: seq[LStmt], inter: var Interpreter, newEnv: Env)


func createUserDefinedLoxFunction(name: string, stm: FuncStmt): LoxCallable =
    let params = stm.params
    let body = stm.body

    let arity = params.len

    var function = LoxFunction()
    function.arity = arity
    function.call = proc(inter: var Interpreter, args: seq[Value]): Value =
        let env = initEnv(inter.globals)
        for i in 0 ..< arity:
            env.define(params[i].lexeme, args[i])


        executeBlock(body, inter, env)

    return function

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


proc evaluate(exp: LxExpr, inter: var Interpreter): Value =
    case exp.kind:
        of ekValue:
            evaluate(exp.val, inter)
        of ekGrouping:
            evaluate(exp.group, inter)
        of ekUnary:
            evaluate(exp.unary, inter)
        of ekBinary:
            evaluate(exp.bin, inter)
        of ekVar:
            # TODO: check on the compilier error message if this is exp.varexp, matching the type of the field.
            # may be able to improve the error message
            evaluate(exp.varex, inter)
        of ekAssign:
            evaluate(exp.assign, inter)
        of ekLogical:
            evaluate(exp.logical, inter)
        of ekCall:
            evaluate(exp.call, inter)


proc evaluate(exp: VarExpr, inter: var Interpreter): Value =
    inter.environment.get(exp.name)

proc evaluate(exp: ValExpr, inter: var Interpreter): Value =
    exp.val

proc evaluate(exp: GroupingExpr, inter: var Interpreter): Value =
    evaluate(exp.lexpr, inter)

proc evaluate(exp: UnaryExpr, inter: var Interpreter): Value =
    let right = evaluate(exp.right, inter)
    case exp.op.typ:
        of tkMinus:
            checkNumberOperand(exp.op, right)
            -right
        of tkBang:
            !right
        else:
            raise newException(Exception, "Invalid unary operator")

proc evaluate(exp: BinExpr, inter: var Interpreter): Value =
    let left = evaluate(exp.left, inter)
    let right = evaluate(exp.right, inter)
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


proc evaluate(exp: AssignExpr, inter: var Interpreter): Value =
    let value = evaluate(exp.value, inter)
    inter.environment.assign(exp.name, value)

    return value


proc evaluate(exp: LogicalExpr, inter: var Interpreter): Value =
    let left = evaluate(exp.left, inter)
    case exp.op.typ:
        of tkOr:
            if isTruthy(left): return left
        of tkAnd:
            if not isTruthy(left): return left
        else:
            raise newException(Exception, "Invalid logical operator")

    return evaluate(exp.right, inter)

proc evaluate(exp: CallExpr, inter: var Interpreter): Value =
    let callee = evaluate(exp.callee, inter)

    var arguments = newSeq[Value]()
    for arg in exp.args:
        arguments.add(evaluate(arg, inter))

    if callee.kind != lkFunction:
        var exception = newException(LoxInvalidCast, "Can only call functions and classes.")
        exception.token = exp.paren
        raise exception

    let function = callee.funcVal

    if arguments.len != function.arity:
        var exception = newException(LoxInvalidCast, "Expected " &
                $function.arity & " arguments but got " & $arguments.len & ".")
        exception.token = exp.paren
        raise exception

    function.call(inter, arguments)




proc execute(s: ExprStmt, inter: var Interpreter) =
    discard evaluate(s.exp, inter)

proc execute(s: PrintStmt, inter: var Interpreter) =
    let v = evaluate(s.exp, inter)
    echo v

proc execute(s: VarStmt, inter: var Interpreter) =
    var value: Value = nil
    if s.init != nil:
        value = evaluate(s.init, inter)

    inter.environment.define(s.name.lexeme, value)

proc executeBlock*(stmts: seq[LStmt], inter: var Interpreter, newEnv: Env) =
    let previous = inter.environment
    try:
        inter.environment = newEnv
        for stm in stmts:
            execute(stm, inter)
    finally:
        inter.environment = previous



proc execute(s: BlockStmt, inter: var Interpreter) =
    executeBlock(s.stmts, inter, initEnv(inter.environment))

proc execute(s: IfStmt, inter: var Interpreter) =
    if isTruthy(evaluate(s.cond, inter)):
        execute(s.thenBranch, inter)
    elif s.elseBranch != nil:
        execute(s.elseBranch, inter)


proc execute(s: WhileStmt, inter: var Interpreter) =
    while isTruthy(evaluate(s.cond, inter)):
        execute(s.body, inter)

proc execute(s: FuncStmt, inter: var Interpreter) =
    let function = createUserDefinedLoxFunction(s.name.lexeme, s)
    inter.environment.define(s.name.lexeme, Value(kind: lkFunction,
            funcVal: function))


proc execute(s: LStmt, inter: var Interpreter) =
    case s.kind:
    of skPrint:
        execute(s.print, inter)
    of skExpr:
        execute(s.exp, inter)
    of skVar:
        execute(s.varstmt, inter)
    of skBlock:
        execute(s.blockstmt, inter)
    of skIf:
        execute(s.ifstmt, inter)
    of skWhile:
        execute(s.whilestmt, inter)
    of skFunc:
        execute(s.funcstmt, inter)


proc interpret*(stmts: seq[LStmt]) =
    var interpreter = Interpreter()
    interpreter.globals = initEnv(nil)
    interpreter.globals.define("clock", clockBuiltin)
    interpreter.environment = interpreter.globals
    try:
        for lstmt in stmts:
            execute(lstmt, interpreter)
    except LoxRuntimeError as e:
        runtimeError(e)

