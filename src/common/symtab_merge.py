from common.symtab import Symtab, VarInfo
from common.compilerSupport import CompileError

def isDefinitelyAssigned[K, T](x: K, nested: list[Symtab[K, T]]) -> bool:
    for st in nested:
        if not st.hasVar(x):
            return False
        if not st.unsafeInfo(x).definitelyAssigned:
            return False
    return True

def merge[K, T](st1: Symtab[K, T], st2: Symtab[K, T]) -> dict[K, VarInfo[T]]:
    envs = [st1, st2]
    union: dict[K, list[VarInfo[T]]] = {}
    for st in envs:
        for k, v in st.items():
            old = union.get(k)
            if old:
                old.append(v)
            else:
                union[k] = [v]
    res: dict[K, VarInfo[T]] = {}
    for x, l in union.items():
        first = l[0]
        rest = l[1:]
        for v in rest:
            if v.ty != first.ty:
                raise CompileError.typeError(f'Inconsistent types for variable {x}')
            if v.scope != first.scope:
                raise CompileError.typeError(f'Inconsistent scope for variable {x}')
        res[x] = VarInfo(first.ty, isDefinitelyAssigned(x, [st1, st2]), first.scope)
    return res
