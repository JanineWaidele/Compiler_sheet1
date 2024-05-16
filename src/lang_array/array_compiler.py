from lang_array.array_astAtom import *
import lang_array.array_ast as plainAst
from common.wasm import *
import lang_array.array_tychecker as array_tychecker
import lang_array.array_transform as array_transform
from lang_array.array_compilerSupport import *
from common.compilerSupport import *
import common.utils as utils


# turning a single statement into a list of Wasm instructions
def stmtToWasm(s: stmt) -> list[WasmInstr]:
    match s:

        case SubscriptAssign(l, i, r):
        # put instructions for the right-hand side, 
        # followed by a i64.store or i32.store after these instructions.
        # TODO: case array and how to get name from l?
            match tyOfExp(r.ty):
                case Array():
                    # TODO
                    st = []
                    pass
                case Int():
                    st = [WasmInstrMem('i64', 'store')]
                case Bool():
                    st = [WasmInstrMem('i32', 'store')]
            return st + expToWasm(r) + expToWasm(AtomExp(l)) # should ty be included in AtomExp(l) ?

        case StmtExp(e):
            return expToWasm(e)

        case Assign(i, e):
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

        case AtomExp(e, _):
            match e:
                case IntConst(val, _):
                    return [WasmInstrConst('i64', val)]
                case BoolConst(val, _):
                    return [WasmInstrConst('i32', val)]
                case Name(var, _):
                    # TODO: is it right?
                    return [WasmInstrVarLocal('set', WasmId(var.name))]

        case ArrayInitDyn(len, elemInit, _):
            # TODO
            res:List[WasmInstr] = []
            l = 0
            match len:
                case IntConst(val,_):
                    l = val
                case BoolConst() | Name():
                    l = 0
            tmp_var = "tmp_i32"
            match elemInit:
                case Name(var,_):
                    for _ in range(l):
                        res += [WasmInstrVarLocal('tee', WasmId(var.name)), WasmInstrVarLocal('get', WasmId(var.name))]
                case IntConst(val,_):
                    for _ in range(l):
                        res += [WasmInstrMem('i64', 'store')]
                        res += expToWasm(AtomExp(elemInit))
                case BoolConst(val,_):
                    for _ in range(l):
                        res += expToWasm(AtomExp(elemInit))
                        #r += arrayOffsetInstrs()
                        res += [WasmInstrConst('i32', 4), WasmInstrNumBinOp('i32', 'add')]
                        res += [WasmInstrVarLocal('set', WasmId(tmp_var))]
            return res

        case ArrayInitStatic(elemInit, _):
            # TODO: is ty needed?
            r: List[WasmInstr] = []
            for e in elemInit:
                match e:
                    case Name(var,_):
                        r += [WasmInstrVarLocal('tee', WasmId(var.name)), WasmInstrVarLocal('get', WasmId(var.name))]
                    case IntConst(val,_):
                        r += [WasmInstrMem('i64', 'store')]
                        r += expToWasm(AtomExp(e))
                    case BoolConst(val,_):
                        r += expToWasm(AtomExp(e))
                        r += [WasmInstrNumBinOp('i32', 'add')]
            return r

        case Subscript(array, i, _):
            # TODO
            return arrayOffsetInstrs(array, i)

        # call function with expressions
        case Call(n,args,t):
            p = '$'+n.name
            f = ''
            match tyOfExp(t):
                case Array():
                    # TODO
                    pass
                case Int():
                    f = 'i64'
                case Bool():
                    f = 'i32'
            match n.name:
                case 'print':
                    p = '$print_'+f
                case 'input_int':
                    p = '$input_'+f 
                case _:
                    pass
            return utils.flatten([expToWasm(e) for e in args])+[WasmInstrCall(WasmId(p))]
        
        # unariy operation (negate a const)
        case UnOp(op,arg):
            match op:
                case USub():
                    return [WasmInstrConst('i64',0)]+expToWasm(arg)+[WasmInstrNumBinOp('i64', 'sub')]
                case Not():
                    # negate by checking if equals False since that is defined as only 0
                    return [WasmInstrConst('i32',0)]+expToWasm(arg)+[WasmInstrIntRelOp('i32', 'eq')]

        # binary operations -> left and right expr on stack, than do mathematic op on them
        case BinOp(l,op,right):
            match op:
                case Is():
                    return []
                case Add():
                    return expToWasm(l)+expToWasm(right)+[WasmInstrNumBinOp('i64', 'add')] 
                case Sub():
                    return expToWasm(l)+expToWasm(right)+[WasmInstrNumBinOp('i64', 'sub')] 
                case Mul():
                    return expToWasm(l)+expToWasm(right)+[WasmInstrNumBinOp('i64', 'mul')] 
                case And():
                    return expToWasm(l) + [WasmInstrIf("i32", expToWasm(right), [WasmInstrConst('i32', 0)])]
                case Or(): 
                    return expToWasm(l) + [WasmInstrIf("i32", [WasmInstrConst('i32', 1)], expToWasm(right))]
                case Less():
                    return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'lt_s')] 
                case LessEq():
                    return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'le_s')]
                case Greater():
                    return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'gt_s')]
                case GreaterEq():
                    return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'ge_s')]
                case Eq():
                    match tyOfExp(l.ty):
                        case Array():
                            # TODO
                            return []
                        case Int():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'eq')]
                        case Bool():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i32', 'eq')]
                case NotEq():
                    match tyOfExp(l.ty):
                        case Array():
                            # TODO
                            return []
                        case Int():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'ne')]
                        case Bool():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i32', 'ne')]
        


def compileStmts(stmts: list[stmt]) -> list[WasmInstr]:
    return utils.flatten([stmtToWasm(s) for s in stmts])

def compileInitArray(lenExp: atomExp, elemTy: ty, cfg: CompilerConfig) -> list[WasmInstr]:
    '''Generates code to initialize an array without initializing the elements.'''
    # TODO
    le = expToWasm(AtomExp(lenExp))[0]
    match le:
        case WasmInstrVarLocal(_,id):
            res: list[WasmInstr] = []
            res += [WasmInstrVarLocal('get', WasmId(id.id)), WasmInstrConst('i64',6553600)]
            res += [WasmInstrIntRelOp('i64','gt_s'), WasmInstrIf('i32', [WasmInstrConst('i32', 0)],[WasmInstrConst('i32', 14)])]
            res += [WasmInstrVarLocal('get', WasmId(id.id)), WasmInstrConst('i64',0)]
            res += [WasmInstrIntRelOp('i64','lt_s'), WasmInstrIf('i32', [WasmInstrConst('i32', 0)],[WasmInstrConst('i32', 14)])]
        case _:
            print("wrong type")
    match elemTy:
        case Array():
            m = 3
            s = 4
        case Int():
            m = 1
            s = 8
        case Bool():
            m = 1
            s = 4
    res: list[WasmInstr] = []
    res += [WasmInstrConvOp('i32.wrap_i64'), WasmInstrConst('i32', 4)]
    res += [WasmInstrNumBinOp('i32','shl'), WasmInstrConst('i32', m), WasmInstrNumBinOp('i32','xor')]
    res += [WasmInstrVarLocal('get', WasmId('$@free_ptr')), WasmInstrMem('i32','store')]
    res += [WasmInstrVarLocal('get', WasmId('$@free_ptr'))]
    # instructions for length
    res += [WasmInstrConvOp('i32.wrap_i64'), WasmInstrConst('i32', s)]
    res += [WasmInstrNumBinOp('i32','mul'), WasmInstrConst('i32',4)]
    res += [WasmInstrNumBinOp('i32','add'), WasmInstrVarLocal('get', WasmId('$@free_ptr'))]
    res += [WasmInstrNumBinOp('i32','add'), WasmInstrVarLocal('set', WasmId('$@free_ptr'))]
    return res

def arrayLenInstrs() -> list[WasmInstr]:
    '''Generates code that expects the array address on top of stack and puts the length on top
    of stack.'''
    # TODO 
    return [WasmInstrMem('i32', 'load'), WasmInstrConst('i32', 4), WasmInstrNumBinOp('i32','shr_u'), WasmInstrConvOp('i64.extend_i32_u')]

def arrayOffsetInstrs(arrayExp: atomExp, indexExp: atomExp) -> list[WasmInstr]:
    '''Returns instructions that places the memory offset for a certain array element on top of
    stack.'''
    # TODO  
    return expToWasm(AtomExp(arrayExp)) + expToWasm(AtomExp(indexExp)) + [WasmInstrNumBinOp('i32', 'add')]

def compileModule(m: plainAst.mod, cfg: CompilerConfig) -> WasmModule:
    # Type check the module
    loc_vars = list(array_tychecker.tycheckModule(m).items())
    # Generate the Wasm module
    wasm_imports = wasmImports(cfg.maxMemSize)
    wasm_exports = [WasmExport('main', WasmExportFunc(WasmId('$main')))]
    locals = [(WasmId('$' + x[0].name), mapTyToWasmValType(x[1].ty)) for x in loc_vars] 
    trans_stmts = array_transform.transStmts(m.stmts,array_transform.Ctx())
    funcs = [WasmFunc(WasmId('$main'), [], None, locals, compileStmts(trans_stmts))]
    return WasmModule(wasm_imports, wasm_exports, globals=[], data=[], funcTable=WasmFuncTable([WasmId('$main')]), funcs=funcs) 

def mapTyToWasmValType(t: ty)->WasmValtype:
    tt: ty = t
    match tt:
        case Array(et):
            # TODO
            at: ty = et
            return mapTyToWasmValType(at)
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
