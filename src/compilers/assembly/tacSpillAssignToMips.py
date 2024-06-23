import assembly.tacSpill_ast as tacSpill
import assembly.mips_ast as mips
from typing import *
from assembly.common import *
from assembly.mipsHelper import *
from common.compilerSupport import *

def primToMips(pri: tacSpill.Prim) -> mips.instr:
    '''convert tacSpill.Prim to mips.instr'''
    match pri.p:
        case tacSpill.Name(n):
            return mips.Label(n.name)
        case tacSpill.Const(i):
            return mips.LoadI(mips.Reg('$t0'), mips.Imm(i))
        
def getRegFromPrim(mi: mips.instr)->mips.Reg:
    '''get register of a tacSpill.Prim'''
    match mi:
        case mips.Label(lstr):
            return mips.Reg(lstr)
        case mips.LoadI(t,_):
            return t
        case _:
            return mips.Reg('$t0')

def getNameFromPrim(pri: tacSpill.Prim)->str:
    '''get name of a tacSpill.Prim'''
    match pri.p:
        case tacSpill.Const():
            return "$t0"
        case tacSpill.Name(nstr):
            return nstr.name


def assignToMips(i: tacSpill.Assign) -> list[mips.instr]:
    #print(i)
    mips_list: list[mips.instr] = []
    match i.left:
        case tacSpill.Prim(pr):
            match pr:
                case tacSpill.Const(ci):
                    #print([mips.LoadI(mips.Reg('$t0'),mips.Imm(ci))])
                    return [mips.LoadI(mips.Reg('$t0'),mips.Imm(ci))]
                case tacSpill.Name(_):
                    # 5: read int
                    mips_list = [mips.LoadI(mips.Reg('$v0'),mips.Imm(5))]
                    #print(mips_list)
                    return mips_list

        case tacSpill.BinOp(l,o,r):
            r_m: mips.instr = primToMips(tacSpill.Prim(l))
            l_m: mips.instr = primToMips(tacSpill.Prim(r))
            match r_m:
                # right exp is Constant
                case mips.LoadI(_,val):
                    match l_m:
                        # left exp is Constant
                        case mips.LoadI(_,val2):
                            if o.name == 'ADD':
                                mips_list += [primToMips(tacSpill.Prim(tacSpill.Const(val.value+val2.value)))]
                            elif o.name == 'SUB':
                                mips_list += [primToMips(tacSpill.Prim(tacSpill.Const(val.value-val2.value)))]
                            elif o.name == 'MUL':
                                #print('mul const const')
                                mips_list += [primToMips(tacSpill.Prim(tacSpill.Const(val.value*val2.value)))]
                        # left exp is Label
                        case mips.Label(_):
                            if o.name == 'ADD':
                                mips_list += [mips.OpI(mips.AddI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val.value))]
                            elif o.name == 'SUB':
                                mips_list += [mips.LoadI(mips.Reg('$t3'), r_m.value)]
                                mips_list += [mips.Op(mips.Sub(), mips.Reg(i.var.name), mips.Reg('$t3'), mips.Reg(r_m.target.name))]
                            elif o.name == 'MUL':
                                #print('mul lab const')
                                mips_list += [mips.LoadI(mips.Reg('$t3'), r_m.value)]
                                mips_list += [mips.Op(mips.Mul(), mips.Reg(i.var.name), mips.Reg('$t3'), mips.Reg(r_m.target.name))]
                            elif o.name == 'Less':
                                mips_list += [mips.OpI(mips.LessI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val.value))]
                        case _:
                            mips_list = []
                    #print(mips_list)
                    return mips_list
                
                # right exp is Label
                case mips.Label(_) :
                    lo = getOpFromName(o.name)
                    match l_m:
                        # left exp is Constant
                        case mips.LoadI(_,val3):
                            if o.name == 'ADD':
                                mips_list += [mips.OpI(mips.AddI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val3.value))]
                            elif o.name == 'SUB':
                                mips_list += [mips.LoadI(mips.Reg('$t3'), val3)]
                                mips_list += [mips.Op(mips.Sub(), mips.Reg(i.var.name), mips.Reg('$t3'), mips.Reg(r_m.label))]
                            elif o.name == 'MUL':
                                #print('mul lab const')
                                mips_list += [mips.LoadI(mips.Reg('$t3'), val3)]
                                mips_list += [mips.Op(mips.Mul(), mips.Reg(i.var.name), mips.Reg('$t3'), mips.Reg(r_m.label))]
                            elif o.name == 'Less':
                                mips_list += [mips.OpI(mips.LessI(), mips.Reg(i.var.name), getRegFromPrim(l_m), mips.Imm(val3.value))]
                        # left exp is Label
                        case mips.Label():
                            if o.name == 'SUB':
                                mips_list += [mips.Op(mips.Sub(), mips.Reg(i.var.name), mips.Reg(l_m.label), mips.Reg(r_m.label))]
                            elif o.name == 'MUL':
                                #print('mul lab lab')
                                mips_list += [mips.Op(mips.Mul(), mips.Reg(i.var.name), mips.Reg(l_m.label), mips.Reg(r_m.label))]
                            else:
                                mips_list += [mips.Op(lo,mips.Reg(i.var.name), getRegFromPrim(l_m), getRegFromPrim(r_m))]
                        case _:
                            pass
                    #print(mips_list)
                    return mips_list
                case _:
                    pass   
            #print(mips_list)
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