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
            return mips.LoadI(mips.Reg('$t0'), mips.Imm(i))
        
def getRegFromPrim(mi: mips.instr)->mips.Reg:
    match mi:
        case mips.Label(lstr):
            return mips.Reg(lstr)
        case mips.LoadI(t,_):
            return t
        case _:
            # t0 is correct
            return mips.Reg('$t0')

def getNameFromPrim(pri: tacSpill.Prim)->str:
    match pri.p:
        case Const():
             # t0 is correct
            return "$t0"
        case Name(nstr):
            return nstr.name


def assignToMips(i: tacSpill.Assign) -> list[mips.instr]:

    mips_list: list[mips.instr] = []
    match i.left:
        case Prim(pr):
            match pr:
                case Const(ci):
                    # t0 is correct
                    return [mips.LoadI(mips.Reg('$t0'),mips.Imm(ci))]
                case Name(_):
                    # 1: print int
                    # 5: read int
                    mips_list = [mips.LoadI(mips.Reg('$v0'),mips.Imm(5))]
                    return mips_list

        case BinOp(l,o,r):
            match o.name:
                case 'ADD' | 'SUB' | 'MUL' | 'Less':
                    r_m: mips.instr = primToMips(tacSpill.Prim(l))
                    l_m: mips.instr = primToMips(tacSpill.Prim(r))
                    match r_m:

                        # constants
                        case mips.LoadI(_,val):
                            # this works
                            match l_m:
                                case mips.LoadI(_,val2):
                                    if o.name == 'ADD':
                                        mips_list = [primToMips(tacSpill.Prim(tacSpill.Const(val.value+val2.value)))]
                                    elif o.name == 'SUB':
                                        mips_list = [primToMips(tacSpill.Prim(tacSpill.Const(val2.value-val.value)))]
                                    elif o.name == 'MUL':
                                        mips_list = [primToMips(tacSpill.Prim(tacSpill.Const(val.value*val2.value)))]
                                case mips.Label(_):
                                    #
                                    if o.name == 'ADD':
                                        mips_list = [mips.OpI(mips.AddI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val.value))]
                                    elif o.name == 'SUB':
                                        # TODO: doesnt work right
                                        mips_list = [mips.OpI(mips.AddI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(-val.value))]
                                    elif o.name == 'Less':
                                        mips_list = [mips.OpI(mips.LessI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val.value))]
                                case _:
                                    mips_list = []
                            return mips_list
                        
                        # labels/input_int
                        case mips.Label(_):
                            #print('here')
                            lo = getOpFromName(o.name)
                            match l_m:
                                case mips.LoadI(_,val3):
                                    if o.name == 'ADD':
                                        mips_list = [mips.OpI(mips.AddI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val3.value))]
                                    elif o.name == 'SUB':
                                        # TODO: doesnt work right
                                        mips_list = [mips.OpI(mips.AddI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(-val3.value))]
                                    elif o.name == 'Less':
                                        mips_list = [mips.OpI(mips.LessI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val3.value))]
                                case mips.Label():
                                    mips_list = [mips.Op(lo,mips.Reg(i.var.name), getRegFromPrim(l_m), getRegFromPrim(r_m))]
                                case _:
                                    mips_list = []
                            # if o.name == 'ADD':
                            #     mips_list = [mips.Op(mips.Add(), mips.Reg(i.var.name), getRegFromPrim(l_m), getRegFromPrim(r_m))]
                            # elif o.name == 'SUB':
                            #     mips_list = [mips.Op(mips.Sub(),mips.Reg(i.var.name), getRegFromPrim(l_m), getRegFromPrim(r_m))]
                            # elif o.name == 'MUL':
                            #     mips_list = [mips.Op(mips.Mul(),mips.Reg(i.var.name), getRegFromPrim(l_m), getRegFromPrim(r_m))]
                            # elif o.name == 'Less':
                            return mips_list
                        case mips.Syscall():
                            print(r_m)
                        case _:
                            pass   
                    return mips_list 
                case _:
                    return mips_list
    
def getOpFromName(op_s: str)->mips.op:
    # o = Add | Sub | Mul | Less | LessEq | Greater | GreaterEq | Eq | NotEq
    o_res = mips.Add()
    match op_s:
        case 'ADD':
            o_res = mips.Add()
        case 'SUB':
            o_res = mips.Sub()
        case 'MUL':
            o_res = mips.Mul()
        case 'Less':
            o_res = mips.Less()
        case 'LessEq':
            o_res = mips.LessEq()
        case 'Greater':
            o_res = mips.Greater()
        case 'GreaterEq':
            o_res = mips.GreaterEq()
        case 'Eq':
            o_res = mips.Eq()
        case 'NotEq': 
            o_res = mips.NotEq()
        case _:
            pass
    return o_res