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
            match l.ty:
                case Int():
                    st = [WasmInstrMem('i64', 'store')]
                case Bool():
                    st = [WasmInstrMem('i32', 'store')]
                case Array(aty):
                    match aty:
                        case Int():
                            st = [WasmInstrMem('i64', 'store')]
                        case Bool():
                            st = [WasmInstrMem('i32', 'store')]
                        case _:
                            st = [WasmInstrMem('i32', 'store')]
                case _:
                    st = [WasmInstrMem('i32', 'store')]
            offs = arrayOffsetInstrs(l, i)
            return offs + expToWasm(r) + st

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
                    return [WasmInstrVarLocal('get', WasmId('$'+var.name))]

        case ArrayInitDyn(l_init, elemInit, ty):
            # TODO
            res:List[WasmInstr] = []
            res += compileInitArray(l_init, tyOfExp(ty), CompilerConfig(1600, 6553600))

            res += [WasmInstrVarLocal('tee', WasmId('$tmp_i32')), WasmInstrVarLocal('get', WasmId('$tmp_i32'))]
            res += [WasmInstrConst('i32', 4), WasmInstrNumBinOp('i32', 'add')]
            res += [WasmInstrVarLocal('set', WasmId('$tmp_i32'))]

            # size of elems in bytes
            s = getByteSize(elemInit)

            # Loop
            loop_instrs:List[WasmInstr] = []
            loop_instrs += [WasmInstrVarLocal('get',WasmId('$tmp_i32')),WasmInstrVarGlobal('get',WasmId('$@free_ptr'))]
            loop_instrs += [WasmInstrIntRelOp('i32','lt_u'),WasmInstrBranch(WasmId('$loop_exit'),False)]
            loop_instrs += [WasmInstrVarLocal('get',WasmId('$tmp_i32'))]
            loop_instrs += expToWasm(AtomExp(l_init))
            loop_instrs += [WasmInstrMem('i64','store')]
            loop_instrs += [WasmInstrVarLocal('get',WasmId('$tmp_i32')), WasmInstrConst('i32',s)]
            loop_instrs += [WasmInstrNumBinOp('i32','add'),WasmInstrVarLocal('set',WasmId('$tmp_i32'))]
            loop_instrs += [WasmInstrBranch(WasmId('$loop_start'),False)]
            # put loop in block
            res += [WasmInstrBlock(WasmId('$loop_exit'), None, [WasmInstrLoop(WasmId('$loop_start'),loop_instrs)])]
            res += [WasmInstrConvOp('i64.extend_i32_s')]     
            return res

        case ArrayInitStatic(elemInit, ty):

            r: List[WasmInstr] = []
            r += compileInitArray(IntConst(len(elemInit)), expType(elemInit[0]), CompilerConfig(1600, 6553600))

            wvt = 'i32'
            s_ars = 4
            #if len(elemInit) > 0:
            ety = elemInit[0].ty
            match ety:
                case Int(): 
                    wvt = 'i64' 
                    s_ars = 8
                case _:
                    pass

            for i, e in enumerate(elemInit):
                
                # read & write local var
                r += [WasmInstrVarLocal('tee', WasmId('$tmp_i32'))]
                r += [WasmInstrVarLocal('get', WasmId('$tmp_i32'))]
                # add offset
                r += [WasmInstrConst('i32', 4 + (s_ars*(i))), WasmInstrNumBinOp('i32','add')]
                # evaluate expr and store it
                r += expToWasm(AtomExp(e))
                
                r += [WasmInstrMem(wvt, 'store')]

            return r

        case Subscript(array, i, ty):
            # TODO
            offset = arrayOffsetInstrs(array, i)
            return offset + [WasmInstrMem(mapTyToWasmValType(tyOfExp(ty)), 'load')]

        # call function with expressions
        case Call(n,args,ct):
            p = '$'+n.name
            f = ''
            f_in = ''
            if len(args) > 0:
                tyTomatch = tyOfExp(args[0].ty)
            else:
                tyTomatch = tyOfExp(ct)
            match tyTomatch:#tyOfExp(args[0].ty):
                case Array():
                    # TODO
                    pass
                case Int():
                    f_in = 'i64'
                    f = 'i64'
                case Bool():
                    f_in = 'i32'
                    f = 'bool'
            match n.name:
                case 'print':
                    p = '$print_'+f
                case 'input_int':
                    p = '$input_'+f_in 
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
                    # TODO: implement equality
                    return expToWasm(l) +expToWasm(right) + [WasmInstrIntRelOp('i64','eq')]
                
                case Add():
                    
                    match tyOfExp(l.ty):
                        case Array():
                            # TODO
                            return []
                        case Int():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrNumBinOp('i64', 'add')]
                        case Bool():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrNumBinOp('i32', 'add')]
    
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

def lengthCheck(lenExp: atomExp, cfg: CompilerConfig) -> list[WasmInstr]:
    # check that length is okay
    le = expToWasm(AtomExp(lenExp))[0]
    length = 0
    match le:
        case WasmInstrConst(_,val):
            match val:
                case int():
                    length = val
                case float():
                    length = int(val)
        case _:
            pass

    if (length > cfg.maxArraySize) or (length < 0):
        return [WasmInstrTrap()] # unreachable
    else:
        return []
    
def computeHeader(lenExp: atomExp, elemTy: ty) -> list[WasmInstr]:
    restmp: list[WasmInstr] = []
    match elemTy:
        case Array():
            m = 3
        case Int():
            m = 1
        case Bool():
            m = 1
    
    # put length of array in storage on top of stack
    restmp += [WasmInstrVarGlobal('get', WasmId('$@free_ptr'))]
    restmp += expToWasm(AtomExp(lenExp))

    # assumes length is on top of stack
    restmp += [WasmInstrConvOp('i32.wrap_i64'), WasmInstrConst('i32', 4)] # convert length to i32 const
    restmp += [WasmInstrNumBinOp('i32','shl'), WasmInstrConst('i32', m), WasmInstrNumBinOp('i32','xor')]
    return restmp

def movePtr(s_bytes: int)->list[WasmInstr]:
    lwas: list[WasmInstr] = []
    lwas += [WasmInstrVarGlobal('get', WasmId('$@free_ptr'))]#, WasmInstrConvOp('i32.wrap_i64')]
    # calc bit length sum for all elements
    lwas += [WasmInstrConst('i32', s_bytes), WasmInstrNumBinOp('i32', 'mul')]
    # add 4 bits needed for header
    lwas += [WasmInstrConst('i32', 4), WasmInstrNumBinOp('i32', 'add')]
    # add space needed by array to ptr and save it -> old ptr val on top of stack
    lwas += [WasmInstrVarGlobal('get', WasmId('$@free_ptr'))]
    lwas += [WasmInstrNumBinOp('i32', 'add')]
    lwas += [WasmInstrVarGlobal('set', WasmId('$@free_ptr'))]
    return lwas

def compileInitArray(lenExp: atomExp, elemTy: ty, cfg: CompilerConfig) -> list[WasmInstr]:
    '''Generates code to initialize an array without initializing the elements.'''
    res: list[WasmInstr] = []
    # 1. length check
    res += lengthCheck(lenExp, cfg)
    # 2. Compute header value
    res += computeHeader(lenExp, elemTy)
    s = getByteSize(lenExp)
    # 3. Store header at $@free_ptr
    res += [WasmInstrVarGlobal('get', WasmId('$@free_ptr')), WasmInstrMem('i32','store')]
    # 4. Move $@free_ptr and return array address
    res += movePtr(s)
    return res

def arrayLenInstrs() -> list[WasmInstr]:
    '''Generates code that expects the array address on top of stack and puts the length on top
    of stack.'''
    # TODO 
    return [WasmInstrMem('i32', 'load'), WasmInstrConst('i32', 4), WasmInstrNumBinOp('i32','shr_u'),WasmInstrConvOp('i64.extend_i32_u')]#

def arrayOffsetInstrs(arrayExp: atomExp, indexExp: atomExp) -> list[WasmInstr]:
    '''Returns instructions that places the memory offset for a certain array element on top of
    stack.'''
    # Compute the base address of the array
    offs: List[WasmInstr] = []
    offs += expToWasm(AtomExp(arrayExp))
    
    # Compute the index offset
    aidx = expToWasm(AtomExp(indexExp))
    offs += aidx
    s = getByteSize(arrayExp)

    offs += [WasmInstrConvOp('i32.wrap_i64'),WasmInstrConst('i32', s)]
    offs += [WasmInstrNumBinOp('i32','mul'),WasmInstrConst('i32', 4)]
    offs += [WasmInstrNumBinOp('i32','add'),WasmInstrNumBinOp('i32','add')] 

    return offs

def compileModule(m: plainAst.mod, cfg: CompilerConfig) -> WasmModule:
    # Type check the module
    loc_vars = list(array_tychecker.tycheckModule(m).items())
    # Generate the Wasm module
    wasm_imports = wasmImports(cfg.maxMemSize)
    wasm_exports = [WasmExport('main', WasmExportFunc(WasmId('$main')))]

    context = array_transform.Ctx()
    trans_stmts = array_transform.transStmts(m.stmts,context)

    locals = [(WasmId('$' + x[0].name), mapTyToWasmValType(x[1].ty)) for x in loc_vars]+ [(WasmId('$tmp_i32'),'i32')]
    locals += [(WasmId('$' + f[0].name), mapTyToWasmValType(f[1])) for f in context.freshVars.items()]
    globals = [WasmGlobal(WasmId('$@free_ptr'),'i32',True,[WasmInstrConst('i32',100)])] 
    
    compiled_stmts = compileStmts(trans_stmts)
    funcs = [WasmFunc(WasmId('$main'), [], None, locals, compiled_stmts)]
    return WasmModule(wasm_imports, wasm_exports, globals=globals, data=[], funcTable=WasmFuncTable([WasmId('$main')]), funcs=funcs) 

def mapTyToWasmValType(t: ty)->WasmValtype:
    tt: ty = t
    match tt:
        case Array():
            return 'i32'
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
        case Void() | None:
            return Int()
        
def expType(ex: exp | atomExp) -> ty:
    match ex.ty:
        case Array() | Bool() | Int():
            return ex.ty
        case NotVoid(nvt):
            return nvt
        case Void() | None:
            raise TypeError

def getByteSize(arE: atomExp):

    match arE.ty:
        case Int():
            s = 8
        case Bool():
            s = 4
        case Array(art):
            match art:
                case Int():
                    s = 8
                case _:
                    s = 4
        case _:
            s = 4
    return s