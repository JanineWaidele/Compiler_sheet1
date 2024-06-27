from lang_fun.fun_astAtom import *
import lang_fun.fun_ast as plainAst
from common.wasm import *
import lang_fun.fun_tychecker as tychecker
import lang_fun.fun_transform as transform
from lang_array.array_compilerSupport import *
from common.compilerSupport import *
import common.utils as utils

FREE_PTR_ID = WasmId("$@free_ptr")

max_array_size = -1

func_table: WasmFuncTable = WasmFuncTable([])

def expToWasm(exp: exp | atomExp) -> list[WasmInstr]:
    match exp:
        case AtomExp(e):
            # done
            return expToWasm(e)
        case IntConst(value):
            # done
            return [WasmInstrConst("i64", value)]
        case BoolConst(value):
            # done
            return [WasmInstrConst("i32", int(value))]
        case VarName(name, ty):
            # done
            return [WasmInstrVarLocal("get", WasmId('$'+name.name))]
        case FunName(fn):
            # done
            if getFuncTabIdx(fn) == -1:
                return [WasmInstrVarLocal("get", WasmId('$'+fn.name))]
            return [WasmInstrConst("i32", getFuncTabIdx(fn))]

        case Call(fun, args, ct):
            # done
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
                case CallTargetIndirect(n, params=params, result=ty):
                    return argInstr + [WasmInstrVarLocal("get", WasmId('$'+n.name)),WasmInstrCallIndirect([mapTyToWasmValType(p) for p in params],resultTyToWasmValTy(ty))]
                
        case UnOp(op, arg):
            # done
            match op:
                case USub():
                    return [WasmInstrConst("i64", 0)] + expToWasm(arg) + [WasmInstrNumBinOp("i64", "sub")]
                case Not():
                    return expToWasm(arg) + [WasmInstrConst("i32", 1), WasmInstrNumBinOp("i32", "xor")]
        # binary operations -> left and right expr on stack, than do mathematic op on them
        case BinOp(l,op,right):
            # done
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
                        
        case ArrayInitDyn(arrLen, elemInit):
            # TODO
            arrayInstr = compileInitArray(arrLen, tyOfExp(elemInit))
            initInstr = arrayInitDynInstrs(elemInit, atomGetArrayTy(elemInit))
            return arrayInstr + initInstr
        case ArrayInitStatic(elemInit):
            # TODO
            arrayInstr = compileInitArray(IntConst(len(elemInit), Int()), tyOfExp(elemInit[0]))
            initInstr = arrayInitInstrs(elemInit)
            return arrayInstr + initInstr
        case Subscript(array, index, ty):
            # done
            if isinstance(ty, tychecker.NotVoid):
                return arrayOffsetInstrs(array, index, ty.ty) + [WasmInstrMem(mapTyToWasmValType(ty.ty), "load")]
            else:
                raise TypeError('ty is void')


def arrayInitInstrs(elemInit: list[atomExp]) -> list[WasmInstr]:
    """
    Expects the address of the array on top of the stack.
    Keeps the address of the array on top of the stack.
    """
    saveArrayStart = [WasmInstrVarLocal("set", WasmId("$@tmp_array_start"))]
    getArrayStart = [WasmInstrVarLocal("get", WasmId("$@tmp_array_start"))]
    addHeaderOffset = [WasmInstrConst("i32", 4), WasmInstrNumBinOp("i32", "add")]
    wast = mapTyToWasmValType(elemInit[0].ty)
    b = 4 if wast == 'i32' else 8

    loadArray: list[WasmInstr] = []
    for i, e in enumerate(elemInit):
        loadArray += (
            (getArrayStart + addHeaderOffset)
            + (  # add element size offset
                [
                    WasmInstrConst("i32", b),
                    WasmInstrConst("i32", i),
                    WasmInstrNumBinOp("i32", "mul"),
                    WasmInstrNumBinOp("i32", "add"),
                ]
            )
            + expToWasm(e)
            + [WasmInstrMem(wast, "store")]
        )

    return saveArrayStart + loadArray + getArrayStart


def arrayInitDynInstrs(elemInit: atomExp, t: ty) -> list[WasmInstr]:
    """
    Expects the address of the array on top of the stack.
    Keeps the address of the array on top of the stack.
    """
    saveArrayStart: list[WasmInstr] = [
        WasmInstrVarLocal("set", WasmId("$@tmp_array_start"))
    ]
    getArrayStart: list[WasmInstr] = [
        WasmInstrVarLocal("get", WasmId("$@tmp_array_start"))
    ]
    addHeaderOffset: list[WasmInstr] = [
        WasmInstrConst("i32", 4),
        WasmInstrNumBinOp("i32", "add"),
    ]

    # create new elements until end of array
    endCond: list[WasmInstr] = getArrayStart + [
        WasmInstrVarGlobal("get", FREE_PTR_ID),
        WasmInstrIntRelOp("i32", "ne"),
    ]
    s = 8 if elemInit.ty == Int() else 4

    move_array_start = getArrayStart + [
        WasmInstrConst("i32", s),
        WasmInstrNumBinOp("i32", "add"),
        WasmInstrVarLocal("set", WasmId("$@tmp_array_start")),
    ]

    putNewElement: list[WasmInstr] = (
        getArrayStart
        + expToWasm(elemInit)
        + [WasmInstrMem(mapTyToWasmValType(t), "store")]
    )

    loop_name = WasmId("$while_loop")
    fillArray = [
        WasmInstrLoop(
            loop_name,
            putNewElement
            + move_array_start
            + endCond
            + [WasmInstrBranch(loop_name, True)],
        )
    ]

    return (
        (  # first time add header to array start
            saveArrayStart
            + getArrayStart
            + getArrayStart
            + addHeaderOffset
            + saveArrayStart
        )
    ) + fillArray


def compileInitArray(lenExp: atomExp, elemTy: ty) -> list[WasmInstr]:
    """
    Generates code to initialize an array without initializing the elements.
    Leaves the address of the array on top of the stack.
    lenExp is an atomic expression computing the length n of the array.
    elemTy is the type of array elements
    """
    lenInstr = expToWasm(lenExp)
    return (
        checkBounds(lenInstr, elemTy)
        + (  # save header at free_ptr
            [WasmInstrVarGlobal("get", FREE_PTR_ID)]
            + (
                lenInstr
                + arrayHeaderInit(is_array_type=isinstance(elemTy, tychecker.Array))
            )
            + [WasmInstrMem("i32", "store")]
        )
        + arrayReserveMemoryInstrs(lenInstr, elemTy)
    )


def arrayReserveMemoryInstrs(lenInstr: list[WasmInstr], t: ty) -> list[WasmInstr]:
    s = 8 if t == Int() else 4

    return (
        [WasmInstrVarGlobal("get", FREE_PTR_ID)]
        + lenInstr
        + [
            WasmInstrConvOp("i32.wrap_i64"),
            WasmInstrConst("i32", s),
            WasmInstrNumBinOp("i32", "mul"),
            WasmInstrConst("i32", 4),
            WasmInstrNumBinOp("i32", "add"),
            WasmInstrVarGlobal("get", FREE_PTR_ID),
            WasmInstrNumBinOp("i32", "add"),
            WasmInstrVarGlobal("set", FREE_PTR_ID),
        ]
    )


def checkBounds(lenInstr: list[WasmInstr], t: ty) ->List[WasmInstr]:
    # done
    """
    lenInstr puts the length of the array on top of the stack.
    Crashes if the array is too small or too big.
    """
    if t == Int():
        s = 8
    else:
        s = 4
    global max_array_size
    max_size = max_array_size // s

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
    # done
    """
    lenInstr puts the length of the array on top of the stack.
    indexInstr puts the desired index
    crashes if index is out of bounds.
    """
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
    """
    Generates code that expects the array address on top of stack and puts the length on top of stack.
    """
    return [
        WasmInstrMem("i32", "load"),
        WasmInstrConst("i32", 4),
        WasmInstrNumBinOp("i32", "shr_u"),
    ]


def arrayHeaderInit(is_array_type: bool) -> list[WasmInstr]:
    """
    expects the array lenght on top of stack
    puts the new array header on top of stack.
    """
    return [
        WasmInstrConvOp("i32.wrap_i64"),
        WasmInstrConst("i32", 4),
        WasmInstrNumBinOp("i32", "shl"),
        WasmInstrConst("i32", 3 if is_array_type else 1),
        WasmInstrNumBinOp("i32", "xor"),
    ]


def arrayOffsetInstrs(arrayExp: atomExp, indexExp: atomExp, t: ty) -> list[WasmInstr]:
    """
    Returns instructions that places the memory offset for a certain array element on top of stack.
    """
    arrayInstrs = expToWasm(arrayExp)
    indexInstrs = expToWasm(indexExp)
    arrayLenInstr = arrayInstrs + arrayLenInstrs()
    s = 8 if t == Int() else 4

    return (
        arrayCheckBoundsIdx(
            arrayLenInstr + [WasmInstrConvOp("i64.extend_i32_u")], indexInstrs
        )
    ) + (  # calculate offset
        arrayInstrs
        + indexInstrs
        + [WasmInstrConvOp("i32.wrap_i64")]
        + [
            WasmInstrConst("i32", s),
            WasmInstrNumBinOp("i32", "mul"),
            WasmInstrConst("i32", 4),
            WasmInstrNumBinOp("i32", "add"),  # offset of the element
            WasmInstrNumBinOp("i32", "add"),  # address of the element
        ]
    )

def stmtToInstrs(stmt: stmt) -> list[WasmInstr]:
    match stmt:
        case StmtExp(exp):
            return expToWasm(exp)
        case Assign(var, right):
            return expToWasm(right) + [WasmInstrVarLocal("set", WasmId('$'+var.name))]
        case IfStmt(cond, thenBody, elseBody):
            return expToWasm(cond) + [
                WasmInstrIf(
                    None,
                    compileStmts(thenBody),
                    compileStmts(elseBody),
                )
            ]
        case WhileStmt(cond, body):
            loop_name = WasmId("$while_loop")
            return expToWasm(cond) + [
                WasmInstrIf(
                    None,
                    [
                        WasmInstrLoop(
                            loop_name,
                            compileStmts(body)
                            + expToWasm(cond)
                            + [WasmInstrBranch(loop_name, True)],
                        )
                    ],
                    [],
                )
            ]
        case SubscriptAssign(left, index, right):
            assert isinstance(left.ty, Array), "Unsupported array type"
            return (
                arrayOffsetInstrs(left, index, left.ty.elemTy)
                + expToWasm(right)
                + [WasmInstrMem(mapTyToWasmValType(left.ty.elemTy), "store")]
            )
        case Return(result):
            match result:
                case None:
                    return [WasmInstrReturn()]
                case _:
                    return expToWasm(result) + [WasmInstrReturn()]

def compileStmts(stmts: list[stmt]) -> list[WasmInstr]:
    return utils.flatten([stmtToInstrs(x) for x in stmts])

def compileModule(m: plainAst.mod, cfg: CompilerConfig) -> WasmModule:
    tyCheckRes = tychecker.tycheckModule(m)
    ctx = transform.Ctx()
    atomStmts = transform.transStmts(m.stmts, ctx)

    global max_array_size
    max_array_size = cfg.maxArraySize

    tll: list[tuple[WasmId, WasmValtype]] = [(WasmId('$'+t.name.name), mapTyToWasmValType(t.ty)) for t in tyCheckRes.toplevelLocals]

    def getAtomLocs() -> list[tuple[WasmId, WasmValtype]]:
        return [(WasmId('$'+n.name), mapTyToWasmValType(t)) for n, t in ctx.freshVars.items()]

    myLocs: list[tuple[WasmId, WasmValtype]] = [(WasmId("$@tmp_array_start"), "i32")]

    allLocs = myLocs + Locals.decls()

    top_level_funcs = [
        (
            WasmFunc(
                id=WasmId("$%"+n.name.name),
                params=[(WasmId('$'+p.var.name), mapTyToWasmValType(p.ty)) for p in n.params],
                result=(resultTyToWasmValTy(n.result)),
                instrs=compileStmts(
                    transform.transStmts(n.body, ctx)
                )  # reuse existing context
                + [WasmInstrTrap()],  # workaround for type-warnings when using return
                locals=allLocs
                + [
                    (
                        WasmId('$'+lvar.name.name),
                        mapTyToWasmValType(lvar.ty),
                    )
                    for lvar in tyCheckRes.funLocals[n.name]
                ]
                + getAtomLocs(),
            )
        )
        for n in m.funs
    ]

    global func_table
    func_table = WasmFuncTable(
        [WasmId(f"$main")] + [WasmId("$%"+n.name.name) for n in m.funs]
    )

    return WasmModule(
        imports=wasmImports(cfg.maxMemSize),
        exports=[WasmExport("main", WasmExportFunc(WasmId("$main")))],
        globals=Globals.decls(),
        data=Errors.data(),
        funcTable=func_table,
        funcs=top_level_funcs
        + [
            WasmFunc(
                id=WasmId(f"$main"),
                params=[],
                result=None,
                locals=tll + allLocs + getAtomLocs(),
                instrs=compileStmts(atomStmts),
            )
        ],
    )

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

def atomGetArrayTy(elemInit: atomExp) -> ty:
    match elemInit:
        case IntConst(_):
            ty = Int()
        case BoolConst(_):
            ty = Bool()
        case VarName(ty=t) | FunName(ty=t):
            ty = t
    return ty


def resultTyToWasmValTy(t: resultTy) -> WasmValtype | None:
    match t:
        case NotVoid():
            return mapTyToWasmValType(t.ty)
        case Void():
            return None


def getFuncTabIdx(name: ident) -> int:
    global func_table
    for i, f in enumerate(func_table.elems):
        if f == WasmId('$%'+name.name):
            return i
    return -1
