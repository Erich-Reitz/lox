import std/tables

import status
import types



proc resolve(r: var Resolver, s: LStmt)
proc resolve(r: var Resolver, exp: LxExpr)

proc beginScope(r: var Resolver) =
    r.scopes.add(initTable[string, bool]())

proc endScope(r: var Resolver) =
    discard r.scopes.pop()

proc declare(r: var Resolver, name: Token) =
    if r.scopes.len == 0:
        return

    if r.scopes[r.scopes.len - 1].contains(name.lexeme):
        error(name, "Variable with this name already declared in this scope.")

    r.scopes[r.scopes.len - 1][name.lexeme] = false

proc define(r: var Resolver, name: Token) =
    if r.scopes.len == 0:
        return

    r.scopes[r.scopes.len - 1][name.lexeme] = true

proc resolve(r: var Resolver, exp: LxExpr, depth: int) =
    r.interpreter.expLocals[exp] = depth



proc resolve(r: var Resolver, stmts: seq[LStmt]) =
    for s in stmts:
        resolve(r, s)


proc visitBlockStmt*(r: var Resolver, s: BlockStmt) =
    beginScope(r)
    resolve(r, s.stmts)
    endScope(r)

proc visitVarStmt*(r: var Resolver, s: VarStmt) =
    declare(r, s.name)
    if s.init != nil:
        resolve(r, s.init)


    define(r, s.name)


# We start at the innermost scope and work outwards,
# looking in each map for a matching name.
# If we find the variable, we resolve it,
# passing in the number of scopes between the current innermost scope
# and the scope where the variable was found.
# So, if the variable was found in the current scope, we pass in 0.
# If it’s in the immediately enclosing scope, 1.
proc resolveLocal(r: var Resolver, exp: LxExpr, name: Token) =
    for i in countdown(r.scopes.len - 1, 0):
        if r.scopes[i].contains(name.lexeme):
            resolve(r, exp, r.scopes.len - 1 - i)
            return

proc visitVarExpr(r: var Resolver, exp: VarExpr, parentExp: LxExpr) =
    if r.scopes.len > 0 and r.scopes[r.scopes.len - 1].getOrDefault(
            exp.name.lexeme, true) == false:
        error(exp.name, "Cannot read local variable in its own initializer.")

    resolveLocal(r, parentExp, exp.name)

proc visitAssignExpr(r: var Resolver, exp: AssignExpr, parentExp: LxExpr) =
    resolve(r, exp.value)
    resolveLocal(r, parentExp, exp.name)

proc resolveFunction(r: var Resolver, f: FuncStmt, typ: FunctionType) =
    let enclosingFunction = r.curfunction
    r.curfunction = typ

    beginScope(r)

    for param in f.params:
        declare(r, param)
        define(r, param)

    resolve(r, f.body)

    endScope(r)
    r.curfunction = enclosingFunction


proc visitFunctionStmt(r: var Resolver, s: FuncStmt) =
    declare(r, s.name)
    define(r, s.name)

    resolveFunction(r, s, ftFunction)

proc visitExpressionStmt(r: var Resolver, s: ExprStmt) =
    resolve(r, s.exp)

proc visitIfStmt(r: var Resolver, s: IfStmt) =
    resolve(r, s.cond)
    resolve(r, s.thenBranch)
    if s.elseBranch != nil:
        resolve(r, s.elseBranch)

proc visitWhileStmt(r: var Resolver, s: WhileStmt) =
    resolve(r, s.cond)
    resolve(r, s.body)

proc visitReturnStmt(r: var Resolver, s: ReturnStmt) =
    if r.curfunction == ftNone:
        error(s.keyword, "Cannot return from top-level code.")

    if s.value != nil:
        if r.curfunction == ftInitializer:
            error(s.keyword, "Cannot return a value from an initializer.")

        resolve(r, s.value)

proc visitPrintStmt(r: var Resolver, s: PrintStmt) =
    resolve(r, s.exp)

proc visitClassStmt(r: var Resolver, s: ClassStmt) =
    let enclosingClass = r.curclass
    r.curclass = ctClass
    declare(r, s.name)
    define(r, s.name)

    if s.superclass != nil and s.name.lexeme == s.superclass.varex.name.lexeme:
        error(s.superclass.varex.name, "A class cannot inherit from itself.")

    if s.superclass != nil:
        r.curclass = ctSubclass
        resolve(r, s.superclass)

    if s.superclass != nil:
        beginScope(r)
        r.scopes[r.scopes.len - 1]["super"] = true

    beginScope(r)
    r.scopes[r.scopes.len - 1]["this"] = true
    for methd in s.methods:
        assert methd.kind == skFunc
        var typ = ftMethod
        if methd.funcstmt.name.lexeme == "init":
            typ = ftInitializer

        resolveFunction(r, methd.funcstmt, typ)


    define(r, s.name)
    endScope(r)
    if s.superclass != nil:
        endScope(r)

    r.curclass = enclosingClass

proc visitBinaryExpr(r: var Resolver, exp: BinExpr) =
    resolve(r, exp.left)
    resolve(r, exp.right)

proc visitCallExpr(r: var Resolver, exp: CallExpr) =
    resolve(r, exp.callee)
    for arg in exp.args:
        resolve(r, arg)

proc visitGroupingExpr(r: var Resolver, exp: GroupingExpr) =
    resolve(r, exp.lexpr)

proc visitLiteralExpr(r: var Resolver, exp: ValExpr) = discard

proc visitLogicalExpr(r: var Resolver, exp: LogicalExpr) =
    resolve(r, exp.left)
    resolve(r, exp.right)

proc visitUnaryExpr(r: var Resolver, exp: UnaryExpr) =
    resolve(r, exp.right)

proc visitGetExpr(r: var Resolver, exp: GetExpr) =
    resolve(r, exp.obj)

proc visitSetExpr(r: var Resolver, exp: SetExpr) =
    resolve(r, exp.value)
    resolve(r, exp.obj)

proc visitThisExpr(r: var Resolver, parentExpr: LxExpr, exp: ThisExpr) =
    if r.curclass == ctNone:
        error(exp.keyword, "Cannot use 'this' outside of a class.")

    resolveLocal(r, parentExpr, exp.keyword)


proc visitSuperExpr(r: var Resolver, parentExpr: LxExpr, exp: SuperExpr) =
    if r.curclass == ctNone:
        error(exp.keyword, "Cannot use 'super' outside of a class.")
    elif r.curclass != ctSubclass:
        error(exp.keyword, "Cannot use 'super' in a class with no superclass.")

    resolveLocal(r, parentExpr, exp.keyword)

proc resolve(r: var Resolver, exp: LxExpr) =
    case exp.kind:
    of ekAssign: visitAssignExpr(r, exp.assign, exp)
    of ekBinary: visitBinaryExpr(r, exp.bin)
    of ekCall: visitCallExpr(r, exp.call)
    of ekGrouping: visitGroupingExpr(r, exp.group)
    of ekValue: visitLiteralExpr(r, exp.val)
    of ekLogical: visitLogicalExpr(r, exp.logical)
    of ekUnary: visitUnaryExpr(r, exp.unary)
    of ekVar: visitVarExpr(r, exp.varex, exp)
    of ekGet: visitGetExpr(r, exp.exget)
    of ekSet: visitSetExpr(r, exp.exset)
    of ekThis: visitThisExpr(r, exp, exp.exthis)
    of ekSuper: visitSuperExpr(r, exp, exp.exsuper)


proc resolve(r: var Resolver, s: LStmt) =
    case s.kind:
    of skExpr: visitExpressionStmt(r, s.exp)
    of skIf: visitIfStmt(r, s.ifstmt)
    of skWhile: visitWhileStmt(r, s.whilestmt)
    of skPrint: visitPrintStmt(r, s.print)
    of skReturn: visitReturnStmt(r, s.returnstmt)
    of skFunc: visitFunctionStmt(r, s.funcstmt)
    of skVar: visitVarStmt(r, s.varstmt)
    of skBlock: visitBlockStmt(r, s.blockstmt)
    of skClass: visitClassStmt(r, s.classstmt)

proc resolve*(r: var Resolver, s: seq[LStmt]) =
    for stm in s:
        resolve(r, stm)



