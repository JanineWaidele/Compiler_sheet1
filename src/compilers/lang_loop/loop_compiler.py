from lang_loop.loop_ast import *
from common.wasm import *
import lang_loop.loop_tychecker as tychecker
from common.compilerSupport import *
import common.utils as utils
from typing import List


# turning a single statement into a list of Wasm instructions
def stmtToWasm(s: stmt) -> list[WasmInstr]:
    match s:

        case StmtExp(e):
            return expToWasm(e)

        case Assign(i,e):
        # Stack:
        # assign to var: z.B. x = exp
        # expression: z.B. 2 + 5
            return expToWasm(e)+[WasmInstrVarLocal('set', WasmId('$'+i.name))]

        case IfStmt(cond, thn, els):
        # Stack:
        # if condition
        # then do smth
        # else do other
            return expToWasm(cond) + [WasmInstrIf(None, compileStmts(thn), compileStmts(els))]
        
        case WhileStmt(cond, bod):
        # Stack:
            return expToWasm(cond) + [WasmInstrIf(None, [WasmInstrLoop(WasmId('$whileLoop'), compileStmts(bod)+ expToWasm(cond) + [WasmInstrBranch(WasmId('$whileLoop'), True)])], [])]
        
# turning an expression into a list of Wasm instructions
def expToWasm(expr: exp) -> list[WasmInstr]:
    
    match expr:

        # define constant
        case IntConst(v):
            return [WasmInstrConst('i64',v)]
        
        # boolean constant -> WasmValType 'i32'
        case BoolConst(v, _):
            return [WasmInstrConst('i32',boolToInt32(v))]
        
        # local.get -> get var with name
        case Name(n):
            return [WasmInstrVarLocal('get', WasmId('$'+n.name))]
        
        # call function with expressions
        case Call(n,args,t):
            p = '$'+n.name
            f = ''
            t_check = t
            if len(args) > 0:
                t_check = args[0].ty

            match tyOfExp(t_check):
                
                case Int():
                    f = 'i64'
                case Bool():
                    f = 'bool'
            match n.name:
                case 'print':
                    p = '$print_'+f
                case 'input_int':
                    p = '$input_'+f 
                case _:
                    pass
            return utils.flatten([expToWasm(e) for e in args])+[WasmInstrCall(WasmId(p))]
        
        # unariy operation ( negate a const)
        case UnOp(op,arg):
            match op:
                case USub():
                    return [WasmInstrConst('i64',0)]+expToWasm(arg)+[WasmInstrNumBinOp('i64', 'sub')]
                case Not():
                    # negate by checking if equals False since that is defined as only 0
                    return [WasmInstrConst('i32',0)]+expToWasm(arg)+[WasmInstrIntRelOp('i32', 'eq')]

        # binary operations -> left and right expr on stack, than do mathematic op on them
        case BinOp(l,op,r):
            match op:
                case Add():
                    return expToWasm(l)+expToWasm(r)+[WasmInstrNumBinOp('i64', 'add')] 
                case Sub():
                    return expToWasm(l)+expToWasm(r)+[WasmInstrNumBinOp('i64', 'sub')] 
                case Mul():
                    return expToWasm(l)+expToWasm(r)+[WasmInstrNumBinOp('i64', 'mul')] 
                case And():
                    return expToWasm(l) + [WasmInstrIf("i32", expToWasm(r), [WasmInstrConst('i32', 0)])]
                case Or(): 
                    return expToWasm(l) + [WasmInstrIf("i32", [WasmInstrConst('i32', 1)], expToWasm(r))]
                case Less():
                    return expToWasm(l)+expToWasm(r)+[WasmInstrIntRelOp('i64', 'lt_s')] 
                case LessEq():
                    return expToWasm(l)+expToWasm(r)+[WasmInstrIntRelOp('i64', 'le_s')]
                case Greater():
                    return expToWasm(l)+expToWasm(r)+[WasmInstrIntRelOp('i64', 'gt_s')]
                case GreaterEq():
                    return expToWasm(l)+expToWasm(r)+[WasmInstrIntRelOp('i64', 'ge_s')]
                case Eq():
                    match tyOfExp(l.ty):
                        case Int():
                            return expToWasm(l)+expToWasm(r)+[WasmInstrIntRelOp('i64', 'eq')]
                        case Bool():
                            return expToWasm(l)+expToWasm(r)+[WasmInstrIntRelOp('i32', 'eq')]
                case NotEq():
                    match tyOfExp(l.ty):
                        case Int():
                            return expToWasm(l)+expToWasm(r)+[WasmInstrIntRelOp('i64', 'ne')]
                        case Bool():
                            return expToWasm(l)+expToWasm(r)+[WasmInstrIntRelOp('i32', 'ne')]
        


def compileStmts(stmts: list[stmt]) -> list[WasmInstr]:
    return utils.flatten([stmtToWasm(s) for s in stmts])

def compileModule(m: mod, cfg: CompilerConfig) -> WasmModule:
    # Type check the module
    loc_vars = list(tychecker.tycheckModule(m).items())
    # Generate the Wasm module
    wasm_imports = wasmImports(cfg.maxMemSize)
    wasm_exports = [WasmExport('main', WasmExportFunc(WasmId('$main')))]
    locals:List[tuple[WasmId, WasmValtype]] = [(WasmId('$'+x[0].name), mapTyToWasmValType(x[1].ty)) for x in loc_vars]
    funcs =[WasmFunc(WasmId('$main'), [], None, locals, compileStmts(m.stmts))]
    return WasmModule(wasm_imports, wasm_exports, globals=[], data=[], funcTable=WasmFuncTable([(WasmId('$main'))]), funcs=funcs)

def mapTyToWasmValType(t: ty)->WasmValtype:
    tt: ty = t
    match tt:
        case Int():
            return 'i64'
        case Bool():
            return 'i32'

def boolToInt32(b: bool)->int:
    if b == False:
        return 0
    elif b == True:
        return 1     

def tyOfExp(e: Optional[resultTy]) -> ty: 

    match e:
        case NotVoid():
            return e.ty
        case Void():
            return Int()
        case None:
            return Int()
