"""
This module provides the transformation from TAC to TACspill.
It first turn the TAC program into a control flow graph, then
computes variable interference graph from, then performs
register allocation by graph coloring, and then assigns
variable to register. Some variables potentially require spilling.

The resulting TACspill program use MIPS register names as variable
names. It uses at most as many $s registers as specified in the
parameter of the function `tacToTacSpill`. Besides $s registers,
it uses three temporary registers $t0, $t1, and $t3,  as well as
some special MIPS registers ($v0, $a0, $sp).

This module relies on the two following two modules to be implemented
by students (for templates see the templates/assembly directory):

- compilers.assembly.liveness
- compilers.assembly.graphColoring
"""

import assembly.tac_ast as tac
import assembly.tacSpill_ast as tacSpill
import assembly.mips_ast as mips
from typing import *
from assembly.common import *
import assembly.controlFlow as controlFlow
import assembly.loopToTac as asCommon
from common.compilerSupport import *
import common.utils as utils

class Regs:
    t1 = tacSpill.Ident('$t0')
    t2 = tacSpill.Ident('$t1')
    t3 = mips.Reg('$t2')
    v0 = mips.Reg('$v0')
    a0 = mips.Reg('$a0')
    sp = mips.Reg('$sp')

def spillIdent(x: tac.ident, regMap: RegisterMap,
               tmp: tacSpill.ident, mode: Literal['load', 'store'] = 'load') \
                   ->  tuple[tacSpill.ident, list[tacSpill.instr]]:
    newX = regMap.resolve(x)
    if newX is not None:
        return (newX, [])
    else:
        match mode:
            case 'load':
                return (tmp, [tacSpill.Unspill(tmp, x.name)])
            case 'store':
                return (tmp, [tacSpill.Spill(tmp, x.name)])

def spillPrim(p: tac.prim, regMap: RegisterMap, tmp: tacSpill.ident) ->  tuple[tacSpill.prim, list[tacSpill.instr]]:
    match p:
        case tac.Const(n): return (tacSpill.Const(n), [])
        case tac.Name(x):
            (newX, instrs) = spillIdent(x, regMap, tmp)
            return (tacSpill.Name(newX), instrs)

def spillExp(e: tac.exp, regMap: RegisterMap) ->  tuple[tacSpill.exp, list[tacSpill.instr]]:
    match e:
        case tac.Prim(p):
            (newP, instrs) = spillPrim(p, regMap, Regs.t1)
            return (tacSpill.Prim(newP), instrs)
        case tac.BinOp(p1, op, p2):
            (newP1, instrs1) = spillPrim(p1, regMap, Regs.t1)
            (newP2, instrs2) = spillPrim(p2, regMap, Regs.t2)
            return (tacSpill.BinOp(newP1, tacSpill.Op(op.name), newP2), instrs1 + instrs2)

def spillIfNeeded(isSpilled: bool, x: tac.ident, newX: tacSpill.ident) -> list[tacSpill.instr]:
    return [tacSpill.Spill(newX, x.name)] if isSpilled else []

def spillInstr(i: tac.instr, regMap: RegisterMap) -> list[tacSpill.instr]:
    match i:
        case tac.Assign(x, e):
            (newE, spillLoads) = spillExp(e, regMap)
            (newX, spillStores) = spillIdent(x, regMap, Regs.t1, 'store')
            return spillLoads + [tacSpill.Assign(newX, newE)] + spillStores
        case tac.Call(x, f, args):
            # Assumptions: all registers in use are callee-save registers (no temporaries)
            # We know that args has at most one argument
            spillLoads: list[tacSpill.instr] = []
            newArgs: list[tacSpill.prim] = []
            for a in args:
                (newA, l) = spillPrim(a, regMap, Regs.t1)
                newArgs.append(newA)
                spillLoads.extend(l)
            if x is not None:
                (newX, spillStores) = spillIdent(x, regMap, Regs.t1, 'store')
            else:
                newX = None
                spillStores = []
            return spillLoads + [tacSpill.Call(newX, tacSpill.Ident(f.name), newArgs)] + spillStores
        case tac.GotoIf(p, label):
            (newP, spillLoads) = spillPrim(p, regMap, Regs.t1)
            return spillLoads + [tacSpill.GotoIf(newP, label)]
        case tac.Goto(label):
            return [tacSpill.Goto(label)]
        case tac.Label(label):
            return [tacSpill.Label(label)]

def tacToTacSpill(instrs: list[tac.instr], maxRegs: int=asCommon.MAX_REGISTERS) -> list[tacSpill.instr]:
    log.info(f'Starting TAC to TACspill transformation, maxRegs={maxRegs}')
    liveness =  utils.importModuleNotInStudent('compilers.assembly.liveness')
    graphColoring = utils.importModuleNotInStudent('compilers.assembly.graphColoring')
    ctrlFlowG = controlFlow.buildControlFlowGraph(instrs)
    log.debug(f'control flow graph: {ctrlFlowG}')
    interfGraph = liveness.buildInterfGraph(ctrlFlowG)
    log.debug(f'interference graph: {interfGraph}')
    regMap = graphColoring.colorInterfGraph(interfGraph, maxRegs=maxRegs)
    log.debug(f'Register map: {regMap}')
    return [x for i in instrs for x in spillInstr(i, regMap)]
