"""
A pretty printer for the MIPS AST.
"""
from assembly.mips_ast import *

def prettyOp(o: op) -> str:
    match o:
        case Add(): return 'add'
        case Sub(): return 'sub'
        case Mul(): return 'mulo'
        case Less(): return 'slt'
        case LessEq(): return 'sle'
        case Greater(): return 'sgt'
        case GreaterEq(): return 'sge'
        case Eq(): return 'eq'
        case NotEq(): return 'sne'

def prettyOpI(o: opI) -> str:
    match o:
        case AddI(): return 'addi'
        case LessI(): return 'slti'

def pr(r: reg) -> str:
    return r.name


def pi(i: imm) -> str:
    return str(i.value)

def mipsPrettyInstr(i: instr) -> str:
    match i:
        case Op(op, r1, r2, r3):
            return f'  {prettyOp(op)} {pr(r1)},{pr(r2)},{pr(r3)}'
        case OpI(op, r1, r2, j):
            return f'  {prettyOpI(op)} {pr(r1)},{pr(r2)},{pi(j)}'
        case LoadWord(r1, off, r2):
            return f'  lw {pr(r1)} {pi(off)}({pr(r2)})'
        case LoadI(r, j):
            return f'  li {pr(r)}, {pi(j)}'
        case LoadA(r, l):
            return f'  la {pr(r)}, {l}'
        case StoreWord(r1, off, r2):
            return f'  sw {pr(r1)} {pi(off)}({pr(r2)})'
        case BranchNeqZero(r, l):
            return f'  bnez {pr(r)}, {l}'
        case Branch(l):
            return f'  b {l}'
        case Move(r1, r2):
            return f'  move {pr(r1)},{pr(r2)}'
        case Syscall():
            return '  syscall'
        case Label(l):
            return f'{l}:'


def mipsPretty(instrs: list[instr]) -> str:
    lines = [mipsPrettyInstr(i) for i in instrs]
    return '\n'.join(lines)
