import lxexpr
import token


type
    StmtKind* = enum skPrint, skExpr, skVar, skBlock, skIf, skWhile

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
