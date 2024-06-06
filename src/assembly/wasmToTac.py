"""
This module implements the translation from Wasm to TAC.
Note that only Wasm instructions required by compiling L_loop are supported.
Entry point for the translation is the function `wasmToTac`
"""
from common.wasm import *
from typing import *
import assembly.tac_ast as tac
from common.utils import assertNotNone

class _Emitter:
    def __init__(self):
        self.instrs: list[tac.instr] = []
        self.regCount: int = 0
        self.labelCount: int = 0
    def emit(self, i: tac.instr):
        self.instrs.append(i)
    def add(self, l: list[tac.instr]):
        self.instrs.extend(l)
    def freshReg(self) -> tac.ident:
        i = self.regCount
        self.regCount = i + 1
        return tac.Ident(f'%R{i}')
    def freshLabel(self, hint: str) -> str:
        i = self.labelCount
        self.labelCount = self.labelCount + 1
        return f'L_{hint}_{i}'

def wasmToTac(instrs: list[WasmInstrL]) -> tuple[Optional[tac.prim], list[tac.instr]]:
    return _toTacR(list(reversed(instrs)))

def _toTacR(rInstrs: list[WasmInstrL]) -> tuple[Optional[tac.prim], list[tac.instr]]:
    e = _Emitter()
    (val, rest) = _toTacSingle(rInstrs, None, e)
    if rest:
        (_, l) = _toTacR(rest)
    else:
        l = []
    return (val, l + e.instrs)

def _callInfo(id: WasmId) -> tuple[int, bool]:
    """
    Given the ID of a function, returns the number of arguments and
    wether the function returns a value.
    """
    match id.id:
        case '$print_i64' | '$print_i32' | '$print_bool': return (1, False)
        case '$input_i64': return (0, True)
        case s:
            raise ValueError(f'Unknown function: {s}')

def downcast(l: list[WasmInstr]) -> list[WasmInstrL]:
    return cast(list[WasmInstrL], l)

def _toTacSingle(rInstrs: list[WasmInstrL], targetVar: Optional[tac.ident], e: _Emitter) -> tuple[Optional[tac.prim], list[WasmInstrL]]:
    match rInstrs:
        case [WasmInstrVarLocal(op, x), *rest]:
            if op == 'get':
                return (tac.Name(tac.Ident(x.id)), rest)
            else:
                tacVar = tac.Ident(x.id)
                (val, rest) = _toTacSingleNotNone(rest, tacVar, e)
                e.emit(tac.Assign(tacVar, tac.Prim(val)))
                if op == 'set':
                    res = None
                else:
                    res = tac.Name(tacVar)
                return (res, rest)
        case [WasmInstrNumBinOp(_, op), *rest] | [WasmInstrIntRelOp(_, op), *rest]:
            (right, rest) = _toTacSingleNotNone(rest, None, e)
            (left, rest) = _toTacSingleNotNone(rest, None, e)
            # no optimization
            opCode = op.upper()
            targetReg = targetVar or e.freshReg()
            e.emit(tac.Assign(targetReg, tac.BinOp(left, tac.Op(opCode), right)))
            return (tac.Name(targetReg), rest)
        case [WasmInstrCall(name), *rest]:
            (n, hasResult) = _callInfo(name)
            args = []
            for _ in range(n):
                (arg, rest) = _toTacSingleNotNone(rest, None, e)
                args = [arg] + args
            if hasResult:
                targetReg = targetVar or e.freshReg()
            else:
                targetReg = None
            e.emit(tac.Call(targetReg, tac.Ident(name.id), args))
            return (tac.Name(targetReg) if targetReg else None, rest)
        case [WasmInstrConst(_, v), *rest]:
            if isinstance(v, int):
                return (tac.Const(v), rest)
            else:
                raise ValueError(f'float constants not supported in TAC')
        case [WasmInstrBranch(target, True), *rest]: # conditional branch
            (val, rest) = _toTacSingleNotNone(rest, None, e)
            e.emit(tac.GotoIf(val, target.id))
            return (None, rest)
        case [WasmInstrBranch(target, False), *rest]: # unconditional branch
            e.emit(tac.Goto(target.id))
            return (None, rest)
        case [WasmInstrIf(_, [], elseInstrs), *rest]:
            (val, rest) = _toTacSingleNotNone(rest, None, e)
            labelEnd = e.freshLabel('end')
            e.emit(tac.GotoIf(val, labelEnd))
            (_, elseInstrsTac) = wasmToTac(downcast(elseInstrs))
            e.add(elseInstrsTac)
            e.emit(tac.Label(labelEnd))
            return (None, rest)
        case [WasmInstrIf(resTy, thenInstrs, elseInstrs), *rest]:
            (val, rest) = _toTacSingleNotNone(rest, None, e)
            targetReg = targetVar or e.freshReg()
            (valElse, elseInstrsTac) = wasmToTac(downcast(elseInstrs))
            (valThen, thenInstrsTac) = wasmToTac(downcast(thenInstrs))
            labelThen = e.freshLabel('then')
            labelEnd = e.freshLabel('end')
            e.emit(tac.GotoIf(val, labelThen))
            e.add(elseInstrsTac)
            if resTy is not None:
                e.emit(tac.Assign(targetReg, tac.Prim(assertNotNone(valElse))))
            e.emit(tac.Goto(labelEnd))
            e.emit(tac.Label(labelThen))
            e.add(thenInstrsTac)
            if resTy is not None:
                e.emit(tac.Assign(targetReg, tac.Prim(assertNotNone(valThen))))
            e.emit(tac.Label(labelEnd))
            if resTy is not None:
                return (tac.Name(targetReg), rest)
            else:
                return (None, rest)
        case [WasmInstrLoop(label, body), *rest]:
            (_, instrsTac) = wasmToTac(downcast(body))
            e.emit(tac.Label(label.id))
            e.add(instrsTac)
            return (None, rest)
        case [WasmInstrBlock(label, resultTy, body), *rest]:
            (val, instrsTac) = wasmToTac(downcast(body))
            e.add(instrsTac)
            if resultTy is not None:
                targetReg = targetVar or e.freshReg()
                e.emit(tac.Assign(targetReg, tac.Prim(assertNotNone(val))))
                e.emit(tac.Label(label.id))
                return (tac.Name(targetReg), rest)
            else:
                e.emit(tac.Label(label.id))
                return (None, rest)
        case []:
            return (None, [])
        case _:
            raise ValueError(f"Don't know what to do with reversed stack {rInstrs}")

def _toTacSingleNotNone(rInstrs: list[WasmInstrL], targetVar: Optional[tac.ident], e: _Emitter) -> tuple[tac.prim, list[WasmInstrL]]:
    (x, rest) = _toTacSingle(rInstrs, targetVar, e)
    if x is None:
        raise ValueError(f'toTacSingle returned None for rInstrs={rInstrs}')
    return (x, rest)
