import assembly.tac_ast as tac
from assembly.tacSpill_ast import * #as tacSpill
import assembly.mips_ast as mips
from typing import *
from assembly.common import *
import assembly.tacInterp as tacInterp
from assembly.mipsHelper import *
from common.compilerSupport import *

def primToMips(pri: tacSpill.Prim) -> mips.instr:
    match pri.p:
        case Name(n):
            return mips.Label(n.name)
        case Const(i):
            return mips.LoadI(mips.Reg('tmp_reg'), mips.Imm(i))
        
def getRegFromPrim(mi: mips.instr)->mips.Reg:
    match mi:
        case mips.Label(lstr):
            return mips.Reg(lstr)
        case mips.LoadI(t,_):
            return t
        case _:
            return mips.Reg('tmp_reg')

def getNameFromPrim(pri: tacSpill.Prim)->str:
    match pri.p:
        case Const():
            return ""
        case Name(nstr):
            return nstr.name


def assignToMips(i: tacSpill.Assign) -> list[mips.instr]:
    # Assign: ident, exp
    # mips.instr: Op | OpI | LoadWord | LoadI | LoadA | StoreWord | BranchNeqZero | Branch | Move | Syscall | Label
    mips_list: list[mips.instr] = []
    match i.left:
        case Prim(pr):
            match pr:
                case Const(ci):
                    #return [mips.StoreWord(mips.Reg(i.var.name), mips.Imm(ci), mips.Reg(i.var.name))]
                    return [mips.LoadI(mips.Reg(i.var.name),mips.Imm(ci))]
                case Name(vn):
                    # 1: print int
                    # 5: read int
                    mips_list = [mips.LoadI(mips.Reg(vn.name),mips.Imm(5))]
                    mips_list += [mips.Syscall(), mips.Move(mips.Reg(vn.name), mips.Reg(i.var.name))]
                    return mips_list

        case BinOp(l,o,r):
            match o.name:
                case 'ADD':
                    r_m: mips.instr = primToMips(tacSpill.Prim(l))
                    l_m: mips.instr = primToMips(tacSpill.Prim(r))
                    match r_m:
                        case mips.LoadI(_,val):
                            mips_list += [mips.OpI(mips.AddI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val.value))]
                            return mips_list
                        case mips.Label(l)| mips.LoadA(_,l):
                            mips_list += [mips.Op(mips.Add(), mips.Reg(i.var.name), getRegFromPrim(l_m), getRegFromPrim(r_m))]
                            return mips_list
                        # case mips.Syscall():
                        #     pass
                        case _:
                            pass   
                    return mips_list 
                case _:
                    return mips_list
    