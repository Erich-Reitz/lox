import math

type
    ValueKind* = enum lkBool, lkNum, lkString, lkIden

    Value* = ref object
        case kind*: ValueKind
        of lkBool: boolVal*: bool
        of lkString, lkIden: strVal*: string
        of lkNum: numVal*: float

func isInteger(value: float, epsilon: float = 1e-10): bool =
    return abs(ceil(value) - value) < epsilon or abs(floor(value) - value) < epsilon

func loxRepr(value: float): string =
    if isInteger(value):
        result = $int(value)
    else:
        result = $value

func `$`*(v: Value): string =
    case v.kind
    of lkBool: result = if v.boolVal == true: "true" else: "false"
    of lkString, lkIden: result = v.strVal
    of lkNum: result = loxRepr(v.numVal)



func isTruthy*(obj: Value): bool =
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



proc `-`*(obj: Value): Value =
    return Value(kind: lkNum, numVal: -obj.numVal)

proc `-`*(l: Value, r: Value): Value =
    return Value(kind: lkNum, numVal: l.numVal - r.numVal)

proc `/`*(l: Value, r: Value): Value =
    return Value(kind: lkNum, numVal: l.numVal / r.numVal)

proc `*`*(l: Value, r: Value): Value =
    return Value(kind: lkNum, numVal: l.numVal * r.numVal)

proc `>`*(l: Value, r: Value): Value =
    return Value(kind: lkBool, boolVal: l.numVal > r.numVal)

proc `>=`*(l: Value, r: Value): Value =
    return Value(kind: lkBool, boolVal: l.numVal >= r.numVal)

proc `<`*(l: Value, r: Value): Value =
    return Value(kind: lkBool, boolVal: l.numVal < r.numVal)

proc `<=`*(l: Value, r: Value): Value =
    return Value(kind: lkBool, boolVal: l.numVal <= r.numVal)

proc `!`*(obj: Value): Value =
    Value(kind: lkBool, boolVal: not isTruthy(obj))

proc `==`*(l: Value, r: Value): Value =
    Value(kind: lkBool, boolVal: isEqual(l, r))

proc `!=`*(l: Value, r: Value): Value =
    Value(kind: lkBool, boolVal: not isEqual(l, r))

