import builtins
import env
import status
import value
import resolver
import types

import std/tables

# function protoypes
proc evaluate(exp: ValExpr, inter: var Interpreter): Value
proc evaluate(exp: GroupingExpr, inter: var Interpreter): Value
proc evaluateUnaryExpr(exp: UnaryExpr, inter: var Interpreter): Value
proc evaluate(exp: BinExpr, inter: var Interpreter): Value
proc evaluateAssignmentExpr(exp: LxExpr, inter: var Interpreter): Value
proc evaluate(exp: LogicalExpr, inter: var Interpreter): Value
proc evaluate(exp: CallExpr, inter: var Interpreter): Value
proc evaluateGetExpr(exp: GetExpr, inter: var Interpreter): Value
proc evaluate(exp: SetExpr, inter: var Interpreter): Value

proc evaluateSuperExpr(exp: LxExpr, inter: var Interpreter): Value

proc evaluateThisExpr(exp: LxExpr, inter: var Interpreter): Value
proc executeStmt(s: LStmt, inter: var Interpreter)

proc executeBlock*(stmts: seq[LStmt], inter: var Interpreter, newEnv: Env)

func createUserDefinedLoxFunction(stm: FuncStmt, closure: Env,
        isInitializer: bool): LoxFunction =
    let params = stm.params
    let body = stm.body

    let arity = params.len

    var function = LoxFunction()
    function.arity = proc(): int = arity
    function.declaration = stm
    function.isInitializer = isInitializer
    # "This is the environment that is active when the function is declared not when it’s called"
    function.closure = closure
    function.call = proc(inter: var Interpreter, args: seq[Value]): Value =
        let env = initEnv(closure)
        for i in 0 ..< arity:
            env.define(params[i].lexeme, args[i])

        try:
            executeBlock(body, inter, env)
        except LoxReturn as ret:
            if isInitializer:
                return closure.getAt(0, "this")
            return ret.value

        if isInitializer:
            return closure.getAt(0, "this");

    return function


proc wrap(i: LoxInstance): Value =
    result = Value(kind: lkInstance, instanceVal: i)

proc wrap(f: LoxFunction): Value =
    result = Value(kind: lkFunction, funcVal: f)


proc instanceSet(instance: var LoxInstance, name: Token, newValue: Value) =
    let key = name.lexeme
    instance.fields[key] = newValue


proc lbind(instance: LoxInstance, methd: LoxFunction): LoxFunction =
    let env = initEnv(methd.closure)
    env.define("this", wrap(instance))
    let wasfuncinit = methd.isInitializer
    createUserDefinedLoxFunction(methd.declaration, env, wasfuncinit)



proc instanceGet(instance: LoxInstance, name: Token): Value =
    if instance.fields.contains(name.lexeme):
        return instance.fields[name.lexeme]

    let methd = instance.class.findMethod(name.lexeme)

    if methd != nil:
        # bind method to our (this)
        let bnd = lbind(instance, methd)
        return wrap(bnd)

    let msg = "Undefined property '" & name.lexeme & "'."

    var exception = newException(LoxUndefinedProperty, msg)
    exception.token = name
    raise exception


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

proc lookupVariable(name: Token, exp: LxExpr, inter: var Interpreter): Value =
    if contains(inter.expLocals, exp):
        let distance = inter.expLocals[exp]
        return inter.environment.getAt(distance, name.lexeme)
    else:
        return inter.globals.get(name)


proc evaluateThisExpr(exp: LxExpr, inter: var Interpreter): Value =
    assert exp.kind == ekThis
    return lookupVariable(exp.exthis.keyword, exp, inter)



proc evaluate(exp: LxExpr, inter: var Interpreter): Value =
    case exp.kind:
        of ekValue:
            evaluate(exp.val, inter)
        of ekGrouping:
            evaluate(exp.group, inter)
        of ekUnary:
            evaluateUnaryExpr(exp.unary, inter)
        of ekBinary:
            evaluate(exp.bin, inter)
        of ekVar:
            # special case because want parent obj
            lookupVariable(exp.varex.name, exp, inter)
        of ekAssign:
            # similar
            evaluateAssignmentExpr(exp, inter)
        of ekLogical:
            evaluate(exp.logical, inter)
        of ekCall:
            evaluate(exp.call, inter)
        of ekGet:
            evaluateGetExpr(exp.exget, inter)
        of ekSet:
            evaluate(exp.exset, inter)
        of ekThis:
            evaluateThisExpr(exp, inter)
        of ekSuper:
            evaluateSuperExpr(exp, inter)


proc evaluate(exp: SetExpr, inter: var Interpreter): Value =
    let lhsObject = evaluate(exp.obj, inter)
    if lhsObject.kind != lkInstance:
        var exception = newException(LoxInvalidCast, "Only instances have fields.")
        exception.token = exp.name
        raise exception

    let rhsValue = evaluate(exp.value, inter)

    lhsObject.instanceVal.instanceSet(exp.name, rhsValue)




proc evaluateGetExpr(exp: GetExpr, inter: var Interpreter): Value =
    let obj = evaluate(exp.obj, inter)
    if obj.kind == lkInstance:
        let res = obj.instanceVal.instanceGet(exp.name)
        return res

    var exception = newException(LoxInvalidCast, "Only instances have properties.")
    exception.token = exp.name
    raise exception

proc evaluate(exp: ValExpr, inter: var Interpreter): Value =
    exp.val

proc evaluate(exp: GroupingExpr, inter: var Interpreter): Value =
    evaluate(exp.lexpr, inter)

proc evaluateUnaryExpr(exp: UnaryExpr, inter: var Interpreter): Value =
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


proc evaluateAssignmentExpr(exp: LxExpr, inter: var Interpreter): Value =
    assert exp.kind == ekAssign
    let value = evaluate(exp.assign.value, inter)

    if contains(inter.expLocals, exp):
        let distance = inter.expLocals[exp]
        inter.environment.assignAt(distance, exp.assign.name, value)
    else:
        inter.globals.assign(exp.assign.name, value)



    return value


proc evaluateSuperExpr(exp: LxExpr, inter: var Interpreter): Value =
    let distance = inter.expLocals[exp]

    let superclass = inter.environment.getAt(distance, "super").classVal

    let obj = inter.environment.getAt(distance - 1, "this").instanceVal

    let mthd = superclass.findMethod(exp.exsuper.methd.lexeme)

    if mthd == nil:
        var exception = newException(LoxUndefinedProperty, "Undefined property '" & exp.exsuper.methd.lexeme & "'.")
        exception.token = exp.exsuper.methd
        raise exception

    let fun = lbind(obj, mthd)

    Value(kind: lkFunction, funcVal: fun)


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

    if callee.kind != lkFunction and callee.kind != lkClass:
        var exception = newException(LoxInvalidCast, "Can only call functions and classes.")
        exception.token = exp.paren
        raise exception


    var function: LoxCallable = nil
    if callee.kind == lkFunction:
        function = callee.funcVal
    else:
        function = callee.classval


    if arguments.len != function.arity():
        var exception = newException(LoxInvalidCast, "Expected " &
                $function.arity() & " arguments but got " & $arguments.len & ".")
        exception.token = exp.paren
        raise exception


    function.call(inter, arguments)




proc visitExprStmt(s: ExprStmt, inter: var Interpreter) =
    discard evaluate(s.exp, inter)

proc visitPrintStmt(s: PrintStmt, inter: var Interpreter) =
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
            executeStmt(stm, inter)
    finally:
        inter.environment = previous



proc execute(s: BlockStmt, inter: var Interpreter) =
    executeBlock(s.stmts, inter, initEnv(inter.environment))

proc execute(s: IfStmt, inter: var Interpreter) =
    if isTruthy(evaluate(s.cond, inter)):
        executeStmt(s.thenBranch, inter)
    elif s.elseBranch != nil:
        executeStmt(s.elseBranch, inter)


proc execute(s: WhileStmt, inter: var Interpreter) =
    while isTruthy(evaluate(s.cond, inter)):
        executeStmt(s.body, inter)

proc execute(s: FuncStmt, inter: var Interpreter) =
    let function = createUserDefinedLoxFunction(s, inter.environment, false)
    inter.environment.define(s.name.lexeme, Value(kind: lkFunction,
            funcVal: function))

proc execute(s: ReturnStmt, inter: var Interpreter) =
    var value: Value = nil
    if s.value != nil:
        value = evaluate(s.value, inter)

    var ret = newException(LoxReturn, "")
    ret.value = value
    raise ret


proc executeClassStmt(c: ClassStmt, inter: var Interpreter) =
    var superclass: Value = nil
    var klass: LoxClass = nil
    if c.superclass != nil:
        superclass = evaluate(c.superclass, inter)
        if superclass.kind != lkClass:
            var exception = newException(LoxInvalidCast, "Superclass must be a class.")
            exception.token = c.superclass.varex.name
            raise exception

        klass = LoxClass(name: c.name.lexeme, superclass: superclass.classVal)
    else:
        klass = LoxClass(name: c.name.lexeme, superclass: nil)


    inter.environment.define(c.name.lexeme, nil)



    klass.arity = proc(): int =
        let initializer = klass.findMethod("init")
        if initializer == nil:
            return 0

        return initializer.arity()

    klass.call = proc(inter: var Interpreter, args: seq[Value]): Value =
        let instance = LoxInstance(class: klass)

        let instanceInit = klass.findMethod("init")
        if instanceInit != nil:
            let bnd = lbind(instance, instanceInit)
            discard bnd.call(inter, args)


        return Value(kind: lkInstance, instanceVal: instance)

    if c.superclass != nil:
        inter.environment = initEnv(inter.environment)
        inter.environment.define("super", superclass)

    for mthd in c.methods:
        assert mthd.kind == skFunc
        let isinit = mthd.funcstmt.name.lexeme == "init"

        let function = createUserDefinedLoxFunction(mthd.funcstmt,
                inter.environment, isinit)
        klass.methods[mthd.funcstmt.name.lexeme] = function

    if c.superclass != nil:
        inter.environment = inter.environment.enclosing

    let classValue = Value(kind: lkClass, classVal: klass)
    inter.environment.assign(c.name, classValue)


proc executeStmt(s: LStmt, inter: var Interpreter) =
    case s.kind:
    of skPrint:
        visitPrintStmt(s.print, inter)
    of skExpr:
        visitExprStmt(s.exp, inter)
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
    of skReturn:
        execute(s.returnstmt, inter)
    of skClass:
        executeClassStmt(s.classstmt, inter)

proc interpret*(stmts: seq[LStmt]) =
    var i = Interpreter()

    var resolver = Resolver(interpreter: i, scopes: newSeq[Table[string, bool]](
        ), curfunction: ftNone, curclass: ctNone)

    resolver.resolve(stmts)

    if hadError:
        return

    i.globals = initEnv(nil)
    i.globals.define("clock", clockBuiltin)
    i.environment = i.globals
    try:
        for lstmt in stmts:
            executeStmt(lstmt, i)
    except LoxRuntimeError as e:
        runtimeError(e)

