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


def assignToMips(i: tacSpill.Assign) -> list[mips.instr]:

    mips_list: list[mips.instr] = []
    match i.left:
        case tacSpill.Prim(pr):
            match pr:
                case tacSpill.Const(ci):
                    mips_list = [mips.LoadI(mips.Reg(i.var.name),mips.Imm(ci))]
                    return mips_list
                case tacSpill.Name(_):
                    mips_list = [mips.LoadI(mips.Reg('$v0'),mips.Imm(5))]
                    return mips_list

        case tacSpill.BinOp(l,o,r):
            r_m: mips.instr = primToMips(tacSpill.Prim(r))
            l_m: mips.instr = primToMips(tacSpill.Prim(l))
            match l_m:
                # left exp is Constant
                case mips.LoadI(_,val):
                    match r_m:
                        # right exp is Constant
                        case mips.LoadI(_,val2):
                            #done
                            if o.name in ['LESS','ADD']:
                                mips_list += [mips.LoadI(mips.Reg('$t3'),val)]
                                mips_list += [mips.OpI(getIOpFromName(o.name),mips.Reg(i.var.name), mips.Reg('$t3'), val2)]
                            else:
                                #
                                mips_list += [mips.LoadI(mips.Reg('$t3'),val)]
                                mips_list += [mips.LoadI(mips.Reg('$t0'),val2)]
                                mips_list += [mips.Op(getOpFromName(o.name), mips.Reg(i.var.name),mips.Reg('$t3'),getRegFromPrim(r_m))]

                        case mips.Label(_):
                            #print('const, label')
                            if o.name in ['ADD','LESS']:
                                # done
                                mips_list += [mips.OpI(getIOpFromName(o.name), mips.Reg(i.var.name), mips.Reg(r_m.label), mips.Imm(val.value))]
                            else:
                                mips_list += [mips.LoadI(mips.Reg('$t3'), l_m.value)]
                                mips_list += [mips.Op(getOpFromName(o.name), mips.Reg(i.var.name), mips.Reg('$t3'), getRegFromPrim(r_m))]
                        case _:
                            pass
                    
                    return mips_list
                
                # left exp is Label
                case mips.Label(labstr) :
                    lo = getOpFromName(o.name)
                    match r_m:

                        # right exp is Constant
                        case mips.LoadI(_,val3):

                            if o.name in ['ADD','LESS']:
                                # current: reg of left, imm of right
                                # testing: left val in t3 -> OPI, ADDI --> addr error#
                                # testing: right val in t3 -> OP, ADD --> changed nothing
                                #mips_list = [mips.LoadI(mips.Reg('$t3'),r_m.value)]#getRegFromPrim(l_m)
                                mips_list += [mips.OpI(getIOpFromName(o.name), mips.Reg(i.var.name), mips.Reg(l_m.label), mips.Imm(val3.value))]
                                #print(mips_list)
                            else:
                                #done
                                mips_list += [mips.LoadI(mips.Reg('$t3'), val3)]
                                mips_list += [mips.Op(lo, mips.Reg(i.var.name), mips.Reg(l_m.label), mips.Reg('$t3'))]

                        # right exp is Label
                        case mips.Label(labstr2):
                            # done?
                            mips_list += [mips.Op(lo, mips.Reg(i.var.name), mips.Reg(labstr), mips.Reg(labstr2))]
                        case _:
                            pass
                    return mips_list
                case _:
                    pass   
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

def getIOpFromName(opi_s: str)->mips.opI:
    opi_res = mips.AddI()
    match opi_s:
        case 'LESS':
            opi_res = mips.LessI()
        case 'ADD':
            opi_res = mips.AddI()
        case _:
            pass
    return opi_res