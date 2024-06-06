"""
This module implements to translation from TACspill to MIPS by performing
instruction selection. Instruction selection for TACspill assign instructions
is expected to be implemented by students in the module
compilers.assembly.tacSpillAssignToMips, see templates/assembly for a
template file.
"""

import assembly.tacSpill_ast as tacSpill
import assembly.mips_ast as mips
from typing import *
from assembly.common import *
from assembly.mipsHelper import *
from common.compilerSupport import *
import common.utils as utils

class StackLocs:
    def __init__(self):
        self._d: dict[str, int] = {}
    def stackOffset(self, name: str) -> int:
        off = self._d.get(name)
        if off is None:
            # We assume that all numbers are 32 bit.
            off = len(self._d) * 4
            self._d[name] = off
        return off

printNewlineInstrs: list[mips.instr] = [
    mips.LoadI(mips.Reg('$v0'), mips.Imm(4)), # system call code for print_str
    mips.LoadA(mips.Reg('$a0'), 'newline'),   # address of newline string
    mips.Syscall()
]

def toMips(i: tacSpill.instr, locs: StackLocs) -> list[mips.instr]:
    match i:
        case tacSpill.Assign():
            m = utils.importModuleNotInStudent('compilers.assembly.tacSpillAssignToMips')
            return m.assignToMips(i)
        case tacSpill.Call(x, f, args):
            prints = ['$print_i64', '$print_i32']
            inputs = ['$input_i64']
            match (x, f.name, args):
                case (None, name, [tacSpill.Const(n)]) if name in prints:
                    return [mips.LoadI(Regs.a0, imm(n)),
                            mips.LoadI(Regs.v0, imm(1)),
                            mips.Syscall()] + printNewlineInstrs
                case (None, name, [tacSpill.Name(y)]) if name in prints:
                    return [mips.Move(Regs.a0, reg(y)),
                            mips.LoadI(Regs.v0, imm(1)),
                            mips.Syscall()] + printNewlineInstrs
                case (y, name, []) if y is not None and name in inputs:
                    return [mips.LoadI(Regs.v0, imm(5)),
                            mips.Syscall(),
                            mips.Move(reg(y), Regs.v0)]
                case _:
                    raise ValueError(f'Invalid call in tacSpill: {i}')
        case tacSpill.GotoIf(tacSpill.Const(n), label):
            tmp = Regs.t3
            return [mips.LoadI(tmp, imm(n)), mips.BranchNeqZero(tmp, label)]
        case tacSpill.GotoIf(tacSpill.Name(y), label):
            return [mips.BranchNeqZero(reg(y), label)]
        case tacSpill.GotoIf(_, _):
            raise ValueError(f'Unhandled GotoIf case: {i}')
        case tacSpill.Goto(label):
            return [mips.Branch(label)]
        case tacSpill.Label(label):
            return [mips.Label(label)]
        case tacSpill.Spill(x, name):
            off = locs.stackOffset(name)
            return [mips.StoreWord(reg(x), imm(off), Regs.sp)]
        case tacSpill.Unspill(x, name):
            off = locs.stackOffset(name)
            return [mips.LoadWord(reg(x), imm(off), Regs.sp)]

def tacSpillToMips(instrs: list[tacSpill.instr]) -> list[mips.instr]:
    locs = StackLocs()
    return [x for i in instrs for x in toMips(i, locs)]
