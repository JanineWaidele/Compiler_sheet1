from lang_fun.fun_astAtom import *
import lang_fun.fun_ast as plainAst
from common.wasm import *
import lang_fun.fun_tychecker as tychecker
import lang_fun.fun_transform as transform
from lang_array.array_compilerSupport import *
from common.compilerSupport import *
import common.utils as utils

maxArraySize = -1

funTab: WasmFuncTable = WasmFuncTable([])

def expToWasm(exp: exp | atomExp) -> list[WasmInstr]:
    match exp:
        case AtomExp(e):
            
            return expToWasm(e)
        case IntConst(value):
            
            return [WasmInstrConst("i64", value)]
        case BoolConst(value):
            
            return [WasmInstrConst("i32", int(value))]
        case VarName(name, ty):
            
            return [WasmInstrVarLocal("get", WasmId('$'+name.name))]
        case FunName(fn):
            
            if getFuncTabIdx(fn) == -1:
                return [WasmInstrVarLocal("get", WasmId('$'+fn.name))]
            return [WasmInstrConst("i32", getFuncTabIdx(fn))]

        case Call(fun, args, ct):
            
            argInstr = utils.flatten([expToWasm(x) for x in args])
            match fun:
                case CallTargetBuiltin(vi):
                    # input int & print    
                    p = '$'+fun.var.name
                    f = ''
                    f_in = ''
                    if len(args) > 0:
                        tyTomatch = TyOfOptResTy(args[0].ty)
                    else:
                        tyTomatch = TyOfOptResTy(ct)

                    match tyTomatch:
                        case Array(_):
                            pass
                        case Fun(_,resT):
                            f = mapTyToWasmValType(TyOfOptResTy(resT))
                        case Int():
                            f_in = 'i64'
                            f = 'i64'
                        case Bool():
                            f_in = 'i32'
                            f = 'bool'
                    match vi.name:
                        case 'print':
                            p = '$print_'+f
                        case 'input_int':
                            p = '$input_'+f_in 
                        case 'len':
                            return (
                                expToWasm(args[0])
                                + arrayLenInstrs()
                                + [WasmInstrConvOp("i64.extend_i32_u")]
                            )
                        case _:
                            p = '$'+vi.name

                    return argInstr + [WasmInstrCall(WasmId(p))]
                case CallTargetDirect(name):
                    return argInstr + [WasmInstrCall(WasmId("$%"+name.name)),]
                case CallTargetIndirect(n, params, ty):
                    return argInstr + [WasmInstrVarLocal("get", WasmId('$'+n.name)),WasmInstrCallIndirect([mapTyToWasmValType(p) for p in params],resultTyToWasmValTy(ty))]
                
        case UnOp(op, arg):
            
            match op:
                case USub():
                    return [WasmInstrConst("i64", 0)] + expToWasm(arg) + [WasmInstrNumBinOp("i64", "sub")]
                case Not():
                    return expToWasm(arg) + [WasmInstrConst("i32", 1), WasmInstrNumBinOp("i32", "xor")]
        # binary operations -> left and right expr on stack, than do mathematic op on them
        case BinOp(l,op,right):
            
            match op:
                case Is():
                    return expToWasm(l) +expToWasm(right) + [WasmInstrIntRelOp('i32','eq')]
                case Add():
                    match TyOfOptResTy(l.ty):
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
                    match TyOfOptResTy(l.ty):
                        case Array() | Fun():
                            return []
                        case Int():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'eq')]
                        case Bool():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i32', 'eq')]
                case NotEq():
                    match TyOfOptResTy(l.ty):
                        case Array() | Fun():
                            # TODO
                            return []
                        case Int():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i64', 'ne')]
                        case Bool():
                            return expToWasm(l)+expToWasm(right)+[WasmInstrIntRelOp('i32', 'ne')]
                        
        case ArrayInitDyn(lenArr, elemInit):

            match elemInit:
                case IntConst():
                    ty = Int()
                case BoolConst():
                    ty = Bool()
                case VarName(_,t) | FunName(_,t):
                    ty = t

            arrinstrs = compileInitArray(lenArr, tyOfExp(elemInit))
            initinstrs = arrayInitDynInstrs(elemInit, ty)
            return arrinstrs + initinstrs
        case ArrayInitStatic(elemInit):
            arrinstrs = compileInitArray(IntConst(len(elemInit), Int()), tyOfExp(elemInit[0]))
            initinstrs = arrayInitInstrs(elemInit)
            return arrinstrs + initinstrs
        case Subscript(array, index, ty):
            
            if isinstance(ty, tychecker.NotVoid):
                return arrayOffsetInstrs(array, index, ty.ty) + [WasmInstrMem(mapTyToWasmValType(ty.ty), "load")]
            else:
                raise TypeError('ty is void')


def arrayInitInstrs(elemInit: list[atomExp]) -> list[WasmInstr]:

    wast = mapTyToWasmValType(elemInit[0].ty)
    b = 4 if wast == 'i32' else 8

    res: list[WasmInstr] = []
    res += [WasmInstrVarLocal("set", WasmId("$@tmp_array_start"))]
    for i, e in enumerate(elemInit):
        res += [WasmInstrVarLocal("get", WasmId("$@tmp_array_start")),WasmInstrConst("i32", 4), WasmInstrNumBinOp("i32", "add")]
        res += [WasmInstrConst("i32", b),WasmInstrConst("i32", i),WasmInstrNumBinOp("i32", "mul")]
        res += [WasmInstrNumBinOp("i32", "add")]+expToWasm(e)+[WasmInstrMem(wast, "store")]
    res += [WasmInstrVarLocal("get", WasmId("$@tmp_array_start"))]
    return res


def arrayInitDynInstrs(elemInit: atomExp, t: ty) -> list[WasmInstr]:

    res = [WasmInstrVarLocal("set", WasmId("$@tmp_array_start"))]
    res += [ WasmInstrVarLocal("get", WasmId("$@tmp_array_start"))]
    res += [ WasmInstrVarLocal("get", WasmId("$@tmp_array_start"))]
    res += [WasmInstrConst("i32", 4),WasmInstrNumBinOp("i32", "add")]
    res += [WasmInstrVarLocal("set", WasmId("$@tmp_array_start"))]


    endcrit: List[WasmInstr] = [WasmInstrVarLocal("get", WasmId("$@tmp_array_start"))]
    endcrit += [WasmInstrVarGlobal("get", WasmId("$@free_ptr")),WasmInstrIntRelOp("i32", "ne")]

    s = 8 if elemInit.ty == Int() else 4

    mov: List[WasmInstr] = [WasmInstrVarLocal("get", WasmId("$@tmp_array_start"))]
    mov += [WasmInstrConst("i32", s),WasmInstrNumBinOp("i32", "add"),WasmInstrVarLocal("set", WasmId("$@tmp_array_start"))]

    newElem: List[WasmInstr] = [WasmInstrVarLocal("get", WasmId("$@tmp_array_start"))]
    newElem += expToWasm(elemInit)
    newElem += [WasmInstrMem(mapTyToWasmValType(t), "store")]
    
    allInstrs: List[WasmInstr] = [WasmInstrLoop(WasmId("$while_loop"),newElem+mov+endcrit+[WasmInstrBranch(WasmId("$while_loop"), True)])]

    return res + allInstrs

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


def checkBounds(lenInstr: list[WasmInstr], t: ty) ->List[WasmInstr]:
    
    if t == Int():
        s = 8
    else:
        s = 4
    global maxArraySize
    max_size = maxArraySize // s

    instRes: list[WasmInstr] = []
    
    instRes += lenInstr
    instRes += [WasmInstrConst('i64',max_size)]
    instRes += [WasmInstrIntRelOp('i64','gt_s')]
    instRes += [WasmInstrIf(None, Errors.outputError("ArraySizeError")+[WasmInstrTrap()], [])]
    
    instRes += lenInstr
    instRes += [WasmInstrConst('i64',0)]
    instRes += [WasmInstrIntRelOp('i64','lt_s')]
    instRes += [WasmInstrIf(None, Errors.outputError("ArraySizeError")+[WasmInstrTrap()], [])]

    return instRes


def arrayCheckBoundsIdx(lenInstr: list[WasmInstr], indexInstr: list[WasmInstr]) -> list[WasmInstr]:
    
    instRes: list[WasmInstr] = []
    
    instRes += indexInstr
    instRes += [WasmInstrConst('i64',0)]
    instRes += [WasmInstrIntRelOp('i64','lt_s')]
    instRes += [WasmInstrIf(None, Errors.outputError("IndexError") + [WasmInstrTrap()], [])]
    
    instRes += indexInstr 
    instRes += lenInstr
    instRes += [WasmInstrIntRelOp('i64','ge_s')]
    instRes += [WasmInstrIf(None, Errors.outputError("IndexError")+[WasmInstrTrap()], [])]
    
    return instRes


def arrayLenInstrs() -> list[WasmInstr]:
    '''Generates code that expects the array address on top of stack and puts the length on top
    of stack.'''
     
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
    offs += arrayCheckBoundsIdx(arre +[WasmInstrConvOp("i64.extend_i32_u")], aidx)
    
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

def stmtToWasm(stmt: stmt) -> list[WasmInstr]:
    match stmt:
        case StmtExp(exp):
            return expToWasm(exp)
        case Assign(var, right):
            return expToWasm(right) + [WasmInstrVarLocal("set", WasmId('$'+var.name))]
        case IfStmt(cond, thenBody, elseBody):
            return expToWasm(cond) + [WasmInstrIf(None,compileStmts(thenBody),compileStmts(elseBody))]
        case WhileStmt(cond, body):
            loop_name = WasmId("$while_loop")
            return expToWasm(cond) + [WasmInstrIf(None,[WasmInstrLoop(loop_name,compileStmts(body)+expToWasm(cond)+[WasmInstrBranch(loop_name, True)])],[])]
        case SubscriptAssign(le, idx, ri):
            if isinstance(le.ty, Array):
                res = arrayOffsetInstrs(le, idx, le.ty.elemTy)
                res += expToWasm(ri)
                res += [WasmInstrMem(mapTyToWasmValType(le.ty.elemTy), "store")]
                return res
            else:
                raise RuntimeError("type of array not supported")
        case Return(result):
            match result:
                case None:
                    return [WasmInstrReturn()]
                case _:
                    return expToWasm(result) + [WasmInstrReturn()]

def compileStmts(stmts: list[stmt]) -> list[WasmInstr]:
    return utils.flatten([stmtToWasm(x) for x in stmts])

def compileModule(m: plainAst.mod, cfg: CompilerConfig) -> WasmModule:
    # in this function help from Anton Kesy
    tyCheckRes = tychecker.tycheckModule(m)
    ctx = transform.Ctx()
    atomStmts = transform.transStmts(m.stmts, ctx)

    global maxArraySize
    maxArraySize = cfg.maxArraySize

    tll: list[tuple[WasmId, WasmValtype]] = [(WasmId('$'+t.name.name), mapTyToWasmValType(t.ty)) for t in tyCheckRes.toplevelLocals]

    def getAtomLocs() -> list[tuple[WasmId, WasmValtype]]:
        return [(WasmId('$'+n.name), mapTyToWasmValType(t)) for n, t in ctx.freshVars.items()]

    myLocs: list[tuple[WasmId, WasmValtype]] = [(WasmId("$@tmp_array_start"), "i32")]
    allLocs = myLocs + Locals.decls()
    topFuns: List[WasmFunc] = []
    for fn in m.funs:
        pams = [(WasmId('$'+p.var.name), mapTyToWasmValType(p.ty)) for p in fn.params]
        topFuns.append( WasmFunc(id=WasmId("$%"+fn.name.name),params=pams,result=(resultTyToWasmValTy(fn.result)),
                instrs=compileStmts(transform.transStmts(fn.body, ctx))+[WasmInstrTrap()],
                locals=allLocs+[(WasmId('$'+lvar.name.name),mapTyToWasmValType(lvar.ty),)for lvar in tyCheckRes.funLocals[fn.name]]+getAtomLocs(),
            ))

    global funTab
    funTab = WasmFuncTable(
        [WasmId(f"$main")] + [WasmId("$%"+n.name.name) for n in m.funs]
    )

    return WasmModule(imports=wasmImports(cfg.maxMemSize),exports=[WasmExport("main", WasmExportFunc(WasmId("$main")))],
        globals=Globals.decls(),data=Errors.data(),funcTable=funTab,funcs=topFuns+[
            WasmFunc(id=WasmId(f"$main"),params=[],result=None,locals=tll + allLocs + getAtomLocs(),instrs=compileStmts(atomStmts))])

def TyOfOptResTy(e: Optional[resultTy]) -> ty: 
    match e:
        case NotVoid():
            return e.ty
        case Void() | None:
            return Int()

def tyOfExp(e: exp | atomExp | plainAst.exp) -> ty:
    match e.ty:
        case NotVoid(t):
            return t
        case Int() | Bool() | Array() | Fun():
            return e.ty
        case _:
            raise NotImplementedError(e, type(e))


def mapTyToWasmValType(t: ty)->WasmValtype:
    tt: ty = t
    match tt:
        case Array() | Fun():
            return 'i32'
        case Int():
            return 'i64'
        case Bool():
            return 'i32'


def resultTyToWasmValTy(t: resultTy) -> WasmValtype | None:
    match t:
        case NotVoid():
            return mapTyToWasmValType(t.ty)
        case Void():
            return None


def getFuncTabIdx(name: ident) -> int:
    global funTab
    for i, f in enumerate(funTab.elems):
        if f == WasmId('$%'+name.name):
            return i
    return -1
