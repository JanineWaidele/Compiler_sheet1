from lang_fun.fun_astAtom import *
import lang_fun.fun_ast as plainAst
from common.wasm import *
import lang_fun.fun_tychecker as fun_tychecker
import lang_fun.fun_transform as fun_transform
from lang_array.array_compilerSupport import *
from common.compilerSupport import *
import common.utils as utils


# turning a single statement into a list of Wasm instructions
def stmtToWasm(s: stmt) -> list[WasmInstr]:
    #print(s)
    match s:

        case SubscriptAssign(l, i, r):

            # put instructions for the right-hand side, 
            # followed by a i64.store or i32.store after these instructions.
            sub_ty = None
            match l.ty:
                case Array(aty):
                    
                    match aty:
                        case Int():
                            st = [WasmInstrMem('i64', 'store')]
                            sub_ty = aty
                        case Bool():
                            st = [WasmInstrMem('i32', 'store')]
                            sub_ty = aty
                        case _:
                            st = [WasmInstrMem('i32', 'store')]
                            sub_ty = aty
                case _:
                    raise TypeError
            offs = arrayOffsetInstrs(l, i, sub_ty)
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
        
        case Return(retRes):
            return expToWasm(getExpFromOpt(retRes)) 

def getExpFromOpt(eo: optional[exp])->exp:
    match eo:
        case AtomExp() | Call() | UnOp() | BinOp() | ArrayInitDyn() | ArrayInitStatic() | Subscript():
            return eo
        case _:
            raise RuntimeError('no exp')
        
def tyOfResultTy(ty: optional[resultTy]) -> ty:
    match ty:
        case NotVoid(t):
            return t
        case Void():
            return Int()
        case _:
            return Int()
        
# turning an expression into a list of Wasm instructions
def expToWasm(expr: exp) -> list[WasmInstr]:

    match expr:

        case AtomExp(e, _):
            # IntConst | BoolConst | VarName | FunName
            match e:
                case IntConst(val, _):
                    return [WasmInstrConst('i64', val)]
                case BoolConst(val, _):
                    return [WasmInstrConst('i32', boolToInt32(val))]
                case VarName(var, _):
                    return [WasmInstrVarLocal('get', WasmId('$'+var.name))]
                case FunName(var, _):
                    return [WasmInstrCall(WasmId('$'+var.name))]

        case ArrayInitDyn(l_init, elemInit, ty):
            # TODO
            res:List[WasmInstr] = []
            
            res += compileInitArray(l_init, expType(elemInit))

            res += [WasmInstrVarLocal('tee', WasmId('$tmp_i32')), WasmInstrVarLocal('get', WasmId('$tmp_i32'))]
            res += [WasmInstrConst('i32', 4), WasmInstrNumBinOp('i32', 'add')]
            res += [WasmInstrVarLocal('set', WasmId('$tmp_i32'))]

            # size of elems in bytes
            s = 8 if elemInit.ty == Int() else 4

            # Loop
            loop_instrs:List[WasmInstr] = []
            loop_instrs += [WasmInstrVarLocal('get',WasmId('$tmp_i32'))]
            loop_instrs += expToWasm(AtomExp(elemInit, ty))
            loop_instrs += [WasmInstrMem(mapTyToWasmValType(expType(elemInit)),'store')]
            loop_instrs += [WasmInstrVarLocal('get',WasmId('$tmp_i32')), WasmInstrConst('i32',s)]
            loop_instrs += [WasmInstrNumBinOp('i32','add'),WasmInstrVarLocal('tee',WasmId('$tmp_i32'))]
            loop_instrs += [WasmInstrVarGlobal('get',WasmId('$@free_ptr'))]
            loop_instrs += [WasmInstrIntRelOp('i32','ne'),WasmInstrBranch(WasmId('$loop_start'),True)]
            # put loop in block
            res += [WasmInstrBlock(WasmId('$loop_exit'), None, [WasmInstrLoop(WasmId('$loop_start'),loop_instrs)])]
            return res

        case ArrayInitStatic(elemInit, ty):

            r: List[WasmInstr] = []
            r += compileInitArray(IntConst(len(elemInit), tyOfResultTy(ty)), expType(elemInit[0]))

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
                r += expToWasm(AtomExp(e, ty))
                
                r += [WasmInstrMem(wvt, 'store')]

            return r

        case Subscript(array, i, ty):
            # TODO
            offset = arrayOffsetInstrs(array, i, tyOfExp(ty))
            return offset + [WasmInstrMem(mapTyToWasmValType(tyOfExp(ty)), 'load')]

        # call function with expressions
        case Call(n,args,ct):
            print(n.var.name)
            # len
            if n.var.name == 'len':
                return expToWasm(args[0]) + arrayLenInstrs() + [WasmInstrConvOp('i64.extend_i32_u')]
            # input int & print    
            p = '$'+n.var.name
            f = ''
            f_in = ''
            if len(args) > 0:
                tyTomatch = tyOfExp(args[0].ty)
            else:
                tyTomatch = tyOfExp(ct)

            match tyTomatch:
                case Array(_):
                    # TODO
                    pass#f = mapTyToWasmValType(eTy)
                case Fun(_,resT):
                    f = mapTyToWasmValType(tyOfExp(resT))
                case Int():
                    f_in = 'i64'
                    f = 'i64'
                case Bool():
                    f_in = 'i32'
                    f = 'bool'
            match n:
                case CallTargetBuiltin(vi):
                    match vi.name:
                        case 'print':
                            p = '$print_'+f
                        case 'input_int':
                            p = '$input_'+f_in 
                        case _:
                            p = '$'+vi.name
                    return utils.flatten([expToWasm(e) for e in args])+[WasmInstrCall(WasmId(p))]
                case CallTargetDirect(ctd_id):
                    #print(args)
                    print(utils.flatten([expToWasm(e) for e in args])+[WasmInstrCall(WasmId('$'+ctd_id.name))])
                    return utils.flatten([expToWasm(e) for e in args])+[WasmInstrCall(WasmId('$'+ctd_id.name))]
                case CallTargetIndirect(cit_v,_,_):
                    return utils.flatten([expToWasm(e) for e in args])+[WasmInstrCall(WasmId('$'+cit_v.name))]
        
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
                    return expToWasm(l) +expToWasm(right) + [WasmInstrIntRelOp('i32','eq')]
                
                case Add():
                    
                    match tyOfExp(l.ty):
                        case Array() | Fun():
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
                        case Array() | Fun():
                            return []
                        case Int():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'eq')]
                        case Bool():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i32', 'eq')]
                case NotEq():
                    match tyOfExp(l.ty):
                        case Array() | Fun():
                            # TODO
                            return []
                        case Int():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'ne')]
                        case Bool():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i32', 'ne')]
        


def compileStmts(stmts: list[stmt]) -> list[WasmInstr]:
    return utils.flatten([stmtToWasm(s) for s in stmts])

def checkBounds(lenInstr: list[WasmInstr], indexInstr: list[WasmInstr]):
    instRes: list[WasmInstr] = []
    
    instRes += [WasmInstrConst('i64',0)]
    instRes += indexInstr
    instRes += [WasmInstrIntRelOp('i64','gt_s')]
    instRes += [WasmInstrIf(None, [WasmInstrConst('i32',14),WasmInstrConst('i32',10),WasmInstrCall(WasmId('$print_err')),WasmInstrTrap()], [])]
    
    instRes += indexInstr 
    instRes += lenInstr
    instRes += [WasmInstrConvOp('i64.extend_i32_u')]
    instRes += [WasmInstrIntRelOp('i64','ge_u')]
    instRes += [WasmInstrIf(None, [WasmInstrConst('i32',14),WasmInstrConst('i32',10),WasmInstrCall(WasmId('$print_err')),WasmInstrTrap()], [])]
    
    return instRes


def lengthCheck(lenExp: atomExp, bytelength: int) -> list[WasmInstr]:

    lRes: list[WasmInstr] = []
    reTy: resultTy = NotVoid(lenExp.ty)
    lRes += [WasmInstrConst('i64',0)]
    lRes += expToWasm(AtomExp(lenExp, reTy))
    lRes += [WasmInstrIntRelOp('i64','gt_s')]
    lRes += [WasmInstrIf(None, [WasmInstrConst('i32',0),WasmInstrConst('i32',14),WasmInstrCall(WasmId('$print_err')),WasmInstrTrap()], [])]
    
    global maxArraySize
    lRes += expToWasm(AtomExp(lenExp, reTy))
    lRes += [WasmInstrConst('i64', maxArraySize//bytelength)]
    lRes += [WasmInstrIntRelOp('i64','ge_u')]
    lRes += [WasmInstrIf(None, [WasmInstrConst('i32',0),WasmInstrConst('i32',14),WasmInstrCall(WasmId('$print_err')),WasmInstrTrap()], [])]
    
    return lRes

    
def computeHeader(lenExp: atomExp, elemTy: ty) -> list[WasmInstr]:
    restmp: list[WasmInstr] = []
    match elemTy:
        case Array() | Fun():
            m = 3
        case Int():
            m = 1
        case Bool():
            m = 1
    
    # put length of array in storage on top of stack
    restmp += [WasmInstrVarGlobal('get', WasmId('$@free_ptr'))]
    reTy: resultTy = NotVoid(lenExp.ty)
    restmp += expToWasm(AtomExp(lenExp, reTy))

    # assumes length is on top of stack
    restmp += [WasmInstrConvOp('i32.wrap_i64'), WasmInstrConst('i32', 4)] # convert length to i32 const
    restmp += [WasmInstrNumBinOp('i32','shl'), WasmInstrConst('i32', m), WasmInstrNumBinOp('i32','xor')]
    return restmp

def movePtr(s_bytes: int, len_instr: list[WasmInstr])->list[WasmInstr]:
    lwas: list[WasmInstr] = []
    lwas += [WasmInstrVarGlobal('get', WasmId('$@free_ptr'))]#
    lwas += len_instr
    lwas += [WasmInstrConvOp('i32.wrap_i64')]
    # calc bit length sum for all elements
    lwas += [WasmInstrConst('i32', s_bytes), WasmInstrNumBinOp('i32', 'mul')]
    # add 4 bits needed for header
    lwas += [WasmInstrConst('i32', 4), WasmInstrNumBinOp('i32', 'add')]
    # add space needed by array to ptr and save it -> old ptr val on top of stack
    lwas += [WasmInstrVarGlobal('get', WasmId('$@free_ptr'))]
    lwas += [WasmInstrNumBinOp('i32', 'add')]
    lwas += [WasmInstrVarGlobal('set', WasmId('$@free_ptr'))]
    return lwas

def compileInitArray(lenExp: atomExp, elemTy: ty) -> list[WasmInstr]:
    '''Generates code to initialize an array without initializing the elements.'''
    res: list[WasmInstr] = []
    reTy: resultTy = NotVoid(lenExp.ty)
    s = 8 if elemTy == Int() else 4
    # 1. length check
    res += lengthCheck(lenExp, s)
    # 2. Compute header value
    res += computeHeader(lenExp, elemTy)
    # 3. Store header at $@free_ptr
    res += [WasmInstrMem('i32','store')]
    # 4. Move $@free_ptr and return array address
    res += movePtr(s, expToWasm(AtomExp(lenExp, reTy)))
    return res

def arrayLenInstrs() -> list[WasmInstr]:
    '''Generates code that expects the array address on top of stack and puts the length on top
    of stack.'''
    # TODO 
    len_instrs: list[WasmInstr] = []
    len_instrs += [WasmInstrMem('i32', 'load'), WasmInstrConst('i32', 4)]
    len_instrs += [WasmInstrNumBinOp('i32','shr_u')]
    return len_instrs

def arrayOffsetInstrs(arrayExp: atomExp, indexExp: atomExp, subTy: ty) -> list[WasmInstr]:
    '''Returns instructions that places the memory offset for a certain array element on top of
    stack.'''
    # Compute the base address of the array
    offs: List[WasmInstr] = []
    reTy: resultTy = NotVoid(subTy)
    arre = expToWasm(AtomExp(arrayExp,reTy)) + arrayLenInstrs()
    
    # Compute the index offset
    aidx = expToWasm(AtomExp(indexExp,reTy))

    # Bounds check
    offs += checkBounds(arre, aidx)
    
    offs +=  expToWasm(AtomExp(arrayExp,reTy))

    offs += aidx
    if subTy == Int():
        s = 8
    else:
        s = 4

    offs += [WasmInstrConvOp('i32.wrap_i64'),WasmInstrConst('i32', s)]
    offs += [WasmInstrNumBinOp('i32','mul'),WasmInstrConst('i32', 4)]
    offs += [WasmInstrNumBinOp('i32','add'),WasmInstrNumBinOp('i32','add')] 

    return offs

def getFuncVarList(llv: list[fun_tychecker.LocalVar])->list[tuple[WasmId, WasmValtype]]:
    # LocalVar: name, ty
    return [(WasmId('$'+il.name.name),mapTyToWasmValType(il.ty)) for il in llv]

def compileModule(m: plainAst.mod, cfg: CompilerConfig) -> WasmModule:
    global maxArraySize
    maxArraySize = cfg.maxArraySize
    # Type check the module
    loc_vars = fun_tychecker.tycheckModule(m).toplevelLocals
    # Generate the Wasm module
    wasm_imports = wasmImports(cfg.maxMemSize)
    wasm_exports = [WasmExport('main', WasmExportFunc(WasmId('$main')))]

    context = fun_transform.Ctx()
    trans_stmts = fun_transform.transStmts(m.stmts,context)

    locals = [(WasmId('$' + x.name.name), mapTyToWasmValType(x.ty)) for x in loc_vars]+ [(WasmId('$tmp_i32'),'i32')]
    locals += [(WasmId('$' + f[0].name), mapTyToWasmValType(f[1])) for f in context.freshVars.items()]
    globals = [WasmGlobal(WasmId('$@free_ptr'),'i32',True,[WasmInstrConst('i32',100)])] 
    
    compiled_stmts = compileStmts(trans_stmts)
    funcs = [WasmFunc(WasmId('$main'), [], None, locals, compiled_stmts)]
    funcs += [WasmFunc(WasmId('$'+i.name), [], None, getFuncVarList(lv), []) for (i,lv) in fun_tychecker.tycheckModule(m).funLocals.items()]
    return WasmModule(wasm_imports, wasm_exports, globals=globals, data=Errors.data(), funcTable=WasmFuncTable([WasmId('$main')]), funcs=funcs) 

def mapTyToWasmValType(t: ty)->WasmValtype:
    tt: ty = t
    match tt:
        case Array() | Fun():
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
        case Array() | Bool() | Int() | Fun():
            return ex.ty
        case NotVoid(nvt):
            return nvt
        case Void():
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