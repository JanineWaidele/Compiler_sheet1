"""
An interpreter for TAC.
"""
from assembly.tac_ast import *
import common.utils as utils
import common.genericCompiler as genCompiler
import assembly.tacPretty as tacPretty
from assembly.loopToTac import loopToTac

type Vars = dict[ident, int]

def evalPrim(p: prim, vars: Vars) -> int:
    match p:
        case Const(v): return v
        case Name(x): return vars[x]

def bi(b: bool) -> int:
    return 1 if b else 0

def evalExp(e: exp, vars: Vars) -> int:
    match e:
        case Prim(p): return evalPrim(p, vars)
        case BinOp(p1, op, p2):
            v1 = evalPrim(p1, vars)
            v2 = evalPrim(p2, vars)
            match op.name:
                case 'ADD': return v1 + v2
                case 'SUB': return v1 - v2
                case 'MUL': return v1 * v2
                case 'EQ': return bi(v1 == v2)
                case 'NE': return bi(v1 != v2)
                case 'LT_S': return bi(v1 < v2)
                case 'GT_S': return bi(v1 > v2)
                case 'LE_S': return bi(v1 <= v2)
                case 'GE_S': return bi(v1 >= v2)
                case s:
                    raise ValueError(f'Unhandled operator: {s}')

def findLabel(instrs: list[instr], label: str) -> int:
    for idx, instr in enumerate(instrs):
        match instr:
            case Label(l) if l == label:
                return idx
            case _:
                pass
    raise ValueError(f'Label {label} not found in {instrs}')

def interpInstrs(instrs: list[instr]):
    pc = 0
    vars: Vars = {}
    while pc < len(instrs):
        instr = instrs[pc]
        match instr:
            case Assign(x, e):
                vars[x] = evalExp(e, vars)
                pc += 1
            case Call(x, fun, args):
                match (fun, args):
                    case (Ident('$input_i64'), []):
                        vars[utils.assertNotNone(x)] = utils.inputInt('Enter some int: ')
                    case (Ident('$print_i32'), [p]) | (Ident('$print_i64'), [p]):
                        print(evalPrim(p, vars))
                    case _:
                        raise ValueError(f'Invalid call: {instr}')
                pc += 1
            case GotoIf(test, label):
                v = evalPrim(test, vars)
                if v != 0:
                    pc = findLabel(instrs, label)
                else:
                    pc += 1
            case Goto(label):
                pc = findLabel(instrs, label)
            case Label(_):
                pc += 1

def interpFile(args: genCompiler.Args, printTac: bool):
    tacInstrs = loopToTac(args)
    if printTac:
        halfDelim = '-----------------------------'
        delim = f'{halfDelim} TAC {halfDelim}'
        print(delim)
        print(tacPretty.prettyInstrs(tacInstrs))
        print(delim)
    interpInstrs(tacInstrs)
