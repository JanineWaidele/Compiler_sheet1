"""
This module exports the function `buildControlFlowGraph` which translates a list
of TAC instructions into a control flow graph.
"""

import assembly.tac_ast as tac
from typing import *
from assembly.common import *
import common.log as log

def _firstBasicBlock(instrs: list[tac.instr], blockIdx: int) -> tuple[BasicBlock, list[tac.instr]]:
    # First, strip off all labels
    labels: list[str] = []
    firstNonLabelIdx = 0
    for idx, instr in enumerate(instrs):
        if not isinstance(instr, tac.Label):
            firstNonLabelIdx = idx
            break
        else:
            labels.append(instr.label)
    else:
        # only labels or no instructions at all
        return (BasicBlock([], blockIdx, labels), [])
    instrs = instrs[firstNonLabelIdx:]
    # Now find the first instruction that is a jump or a label
    firstJumpOrLabelIdx = 0
    for idx, instr in enumerate(instrs):
        if not isinstance(instr, tac.Assign) and not isinstance(instr, tac.Call):
            firstJumpOrLabelIdx = idx
            break
    else:
        # instrs only contains assign or calls
        return (BasicBlock(instrs, blockIdx, labels), [])
    # idx is the index of the first instruction that is a jump or a label.
    offset = 0 if isinstance(instrs[firstJumpOrLabelIdx], tac.Label) else 1
    # jumps are part of the basic block, labels not
    basicInstrs = instrs[:firstJumpOrLabelIdx+offset]
    rest = instrs[firstJumpOrLabelIdx+offset:]
    return (BasicBlock(basicInstrs, blockIdx, labels), rest)

def buildControlFlowGraph(instrs: list[tac.instr]) -> ControlFlowGraph:
    g = Graph[int, BasicBlock]('directed')
    idx = 0
    labelToIdx: dict[str, int] = {}
    while instrs:
        (bb, instrs) = _firstBasicBlock(instrs, idx)
        log.debug(f'{bb}')
        g.addVertex(idx, bb)
        for l in bb.labels:
            labelToIdx[l] = idx
        idx += 1
    for bb in g.values:
        succs: list[int] = []
        match bb.last:
            case tac.Goto(label):
                succs.append(labelToIdx[label])
            case tac.GotoIf(_, label):
                succs.append(labelToIdx[label])
                nextIdx = bb.index + 1
                if g.hasVertex(nextIdx):
                    succs.append(nextIdx)
            case _:
                nextIdx = bb.index + 1
                if g.hasVertex(nextIdx):
                    succs.append(nextIdx)
        for s in succs:
            g.addEdge(bb.index, s)
    return g
