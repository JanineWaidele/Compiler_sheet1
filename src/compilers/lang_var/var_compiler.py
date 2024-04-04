from lang_var.var_ast import *
from common.wasm import *
import lang_var.var_tychecker as var_tychecker
from common.compilerSupport import *
import common.utils as utils
from typing import List


# turning a single statement into a list of Wasm instructions
def stmtToWasm(s: stmt) -> list[WasmInstr]:
    match s:
        case StmtExp(e):
            return expToWasm(e)
        case Assign(i,e):
            return expToWasm(e)+[WasmInstrVarLocal('set', WasmId('$'+i.name))]
        
# turning an expression into a list of Wasm instructions
def expToWasm(expr: exp) -> list[WasmInstr]:
    
    match expr:

        # define constant
        case IntConst(v):
            return [WasmInstrConst('i64',v)]
        
        # local.get -> get var with name
        case Name(n):
            return [WasmInstrVarLocal('get', WasmId('$'+n.name))]
        
        # call function with expressions
        case Call(n,args):
            t = '$'+n.name
            match n.name:
                case 'print':
                    t = '$print_i64'
                case 'input_int':
                    t = '$input_i64'     
                case _:
                    pass
            return utils.flatten([expToWasm(e) for e in args])+[WasmInstrCall(WasmId(t))]
        
        # unariy operation ( negate a const)
        case UnOp(op,arg):
            return [WasmInstrConst('i64',0)]+expToWasm(arg)+[WasmInstrNumBinOp('i64', 'sub')]

        # binary operations -> left and right expr on stack, than do mathematic op on them
        case BinOp(l,op,r):
            litOp = ''
            match op:
                case Add():
                    litOp = 'add'
                case Sub():
                    litOp = 'sub'
                case Mul():
                    litOp = 'mul' 
            return expToWasm(l)+expToWasm(r)+[WasmInstrNumBinOp('i64', litOp)] 

def compileStmts(stmts: list[stmt]) -> list[WasmInstr]:
    return utils.flatten([stmtToWasm(s) for s in stmts])

def compileModule(m: mod, cfg: CompilerConfig) -> WasmModule:
    # Type check the module
    loc_vars = var_tychecker.tycheckModule(m)
    # Generate the Wasm module
    wasm_imports = wasmImports(cfg.maxMemSize)
    wasm_exports = [WasmExport('main', WasmExportFunc(WasmId('$main')))]
    locals:List[tuple[WasmId, WasmValtype]] = [(WasmId('$'+x.name), 'i64') for x in loc_vars]
    funcs = [WasmFunc(WasmId('$main'), [], None, locals, compileStmts(m.stmts))]
    return WasmModule(wasm_imports, wasm_exports, globals=[], data=[], funcTable=WasmFuncTable([]), funcs=funcs)
    