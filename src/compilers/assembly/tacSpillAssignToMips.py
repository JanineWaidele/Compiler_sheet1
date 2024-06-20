import assembly.tac_ast as tac
from assembly.tacSpill_ast import * #as tacSpill
import assembly.mips_ast as mips
from typing import *
from assembly.common import *
import assembly.tacInterp as tacInterp
from assembly.mipsHelper import *
from common.compilerSupport import *

def primToMips(pri: tacSpill.Prim) -> list[mips.instr]:
    prim_list: list[mips.instr] = []
    match pri.p:
        case Name(n):
            prim_list += [mips.Label(n.name)]
            return prim_list
        case Const(i):
            prim_list += [mips.LoadI()]

def assignToMips(i: tacSpill.Assign) -> list[mips.instr]:
    # Assign: ident, exp
    # mips.instr: Op | OpI | LoadWord | LoadI | LoadA | StoreWord | BranchNeqZero | Branch | Move | Syscall | Label
    mips_list: list[mips.instr] = []
    match i.left:
        case Prim():
            pass
        case BinOp(l,o,r):
            match o:
                case Op('add'):
                    mips_list += [mips.Op(mips.AddI(), assignToMips(l), assignToMips(r))]
                    return mips_list
                    match l.var:
                        case Ident(n):
                            mips.Op(mips.Add(),mips.Reg(i.var.name),mips.Reg(n),mips.Reg(r.var.name))
                        case _:
                            pass