import lexceptions
import lxexpr
import status
import token
import value

proc isTruthy(obj: Value): bool =
    if obj == nil:
        return false
    if obj.kind == lkBool:
        return obj.boolVal

    return true


proc isEqual(a: Value, b: Value): bool =
    if a == nil and b == nil:
        return true
    if a == nil:
        return false

    if a.kind != b.kind:
        return false

    case a.kind:
        of lkBool:
            return a.boolVal == b.boolVal
        of lkNum:
            return a.numVal == b.numVal
        of lkString:
            return a.strVal == b.strVal
        of lkIden:
            echo "Warning: Comparing two identifiers"
            return a.strVal == b.strVal



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

proc `-`(obj: Value): Value =
    return Value(kind: lkNum, numVal: -obj.numVal)


proc `-`(l: Value, r: Value): Value =
    return Value(kind: lkNum, numVal: l.numVal - r.numVal)

proc `/`(l: Value, r: Value): Value =
    return Value(kind: lkNum, numVal: l.numVal / r.numVal)

proc `*`(l: Value, r: Value): Value =
    return Value(kind: lkNum, numVal: l.numVal * r.numVal)

proc `>`(l: Value, r: Value): Value =
    return Value(kind: lkBool, boolVal: l.numVal > r.numVal)

proc `>=`(l: Value, r: Value): Value =
    return Value(kind: lkBool, boolVal: l.numVal >= r.numVal)

proc `<`(l: Value, r: Value): Value =
    return Value(kind: lkBool, boolVal: l.numVal < r.numVal)

proc `<=`(l: Value, r: Value): Value =
    return Value(kind: lkBool, boolVal: l.numVal <= r.numVal)

proc `!`(obj: Value): Value =
    Value(kind: lkBool, boolVal: not isTruthy(obj))

proc evaluate(exp: ValExpr): Value
proc evaluate(exp: GroupingExpr): Value
proc evaluate(exp: UnaryExpr): Value
proc evaluate(exp: BinExpr): Value

proc evaluate(exp: LxExpr): Value =
    case exp.kind:
        of ekValue:
            evaluate(exp.val)
        of ekGrouping:
            evaluate(exp.group)
        of ekUnary:
            evaluate(exp.unary)
        of ekBinary:
            evaluate(exp.bin)


proc evaluate(exp: ValExpr): Value =
    exp.val

proc evaluate(exp: GroupingExpr): Value =
    evaluate(exp.lexpr)

proc evaluate(exp: UnaryExpr): Value =
    let right = evaluate(exp.right)
    case exp.op.typ:
        of tkMinus:
            checkNumberOperand(exp.op, right)
            -right
        of tkBang:
            !right
        else:
            raise newException(Exception, "Invalid unary operator")

proc evaluate(exp: BinExpr): Value =
    let left = evaluate(exp.left)
    let right = evaluate(exp.right)
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
            return Value(kind: lkBool, boolVal: not isEqual(left, right))
        of tkEqualEqual:
            return Value(kind: lkBool, boolVal: isEqual(left, right))
        else:
            raise newException(Exception, "Invalid binary operator")

proc interpret*(exp: LxExpr) =
    try:
        let res = evaluate(exp)
        echo res
    except LoxRuntimeError as e:
        runtimeError(e)

