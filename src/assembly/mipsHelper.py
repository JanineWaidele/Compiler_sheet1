"""
Module with some helpers for MIPS assembly.
"""
import assembly.tacSpill_ast as tacSpill
import assembly.mips_ast as mips
from typing import *
from assembly.common import *
from common.compilerSupport import *

class Regs:
    t1 = tacSpill.Ident('$t0')
    t2 = tacSpill.Ident('$t1')
    t3 = mips.Reg('$t2')
    v0 = mips.Reg('$v0')
    a0 = mips.Reg('$a0')
    sp = mips.Reg('$sp')

def reg(x: tacSpill.ident) -> mips.reg:
    return mips.Reg(x.name)

def imm(i: int) -> mips.imm:
    if i < -2**15 or i > 2.**15 - 1:
        raise ValueError(f'Constant too large: {i}')
    return mips.Imm(i)
