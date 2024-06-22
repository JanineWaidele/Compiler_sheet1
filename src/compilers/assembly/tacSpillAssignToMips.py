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
            return "tmp_const"
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
                    print(f"i_const: {i}")
                    return [mips.LoadI(mips.Reg('$s1'),mips.Imm(ci))]#,mips.Syscall(),mips.Move(mips.Reg('$s1'),mips.Reg(i.var.name))]
                case Name(vn):
                    # 1: print int
                    # 5: read int
                    mips_list = [mips.LoadI(mips.Reg('$v0'),mips.Imm(5))]
                    return mips_list

        case BinOp(l,o,r):
            match o.name:
                case 'ADD' | 'SUB':
                    r_m: mips.instr = primToMips(tacSpill.Prim(l))
                    l_m: mips.instr = primToMips(tacSpill.Prim(r))
                    match r_m:
                        # constants
                        case mips.LoadI(_,val):
                            mips_list = [mips.OpI(mips.AddI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val.value))]
                            return mips_list
                        # labels/input_int
                        case mips.Label(l)| mips.LoadA(_,l):
                            if o.name == 'ADD':
                                mips_list = [mips.Op(mips.Add(), mips.Reg(i.var.name), getRegFromPrim(l_m), getRegFromPrim(r_m))]
                            else:
                                mips_list = [mips.Op(mips.Sub(), mips.Reg(i.var.name), getRegFromPrim(l_m), getRegFromPrim(r_m))]
                            return mips_list
                        case mips.Syscall():
                            print(r_m)
                        case _:
                            pass   
                    return mips_list 
                case _:
                    return mips_list
    