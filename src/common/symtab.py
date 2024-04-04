from __future__ import annotations
from dataclasses import dataclass
from common.compilerSupport import CompileError
import common.log as log
import pprint
from typing import *

# Scoping rules:
# (1) Block constructs do not create a new scope (because that's how it is in python).
# (2) A variables has only one type.
# (3) A variable can only be used if it has been definitely assigned.
#
# Some examples:
#
# x = 1
# if ...:
#     x = True # not allowed because x has different types, see test_files/lang_var/scoping_11.py
#
# if ...:
#    x = True
# else ...:
#    x = 1 # not allowed because x has different types,
#          # see test_files/lang_var/scoping_07.py and .../scoping_08.py
#
# if ...:
#    x = 2
# else ...:
#    x = 1
# print(x) # ok, see test_files/lang_var/scoping_02.py and .../scoping_09.py
#
# if ...:
#     x = 1
# else:
#     ... # no assignment to x
# x = 2
# print(x) # ok, see test_files/lang_var/scoping_13.py

type Scope = Literal['var', 'fun']

@dataclass(frozen=True)
class VarInfo[T]:
    ty: T
    definitelyAssigned: bool
    scope: Scope

class Symtab[K, T]:
    def __init__(self):
        self.__vars: dict[K, VarInfo[T]] = {}
    def __repr__(self):
        return f'Symtab({self.__vars})'
    def assign(self, var: K, ty: T, scope: Scope = 'var'):
        info = self.__vars.get(var)
        if info and ty != info.ty:
            raise CompileError.typeError(
                f'Inconsistent types for variable {var}: {info.ty} and {ty}')
        if info and info.scope == 'fun':
            raise CompileError.typeError(f'Cannot re-assign global function variable {var}')
        self.__vars[var] = VarInfo(ty, True, scope)
    def use(self, var: K) -> T:
        return self.info(var).ty
    def scope(self, var: K) -> Scope:
        return self.info(var).scope
    def unsafeInfo(self, var: K) -> VarInfo[T]:
        return self.__vars[var]
    def items(self) -> Iterable[tuple[K, VarInfo[T]]]:
        return self.__vars.items()
    def info(self, var: K) -> VarInfo[T]:
        if var not in self.__vars:
            log.debug(f"Symtab: {pprint.pformat(self.__vars)}")
            raise CompileError.typeError(f'Unknown variable: {var}')
        info = self.__vars[var]
        if not info.definitelyAssigned:
            raise CompileError.typeError(f'Variable {var} might not have been initialized')
        return info
    def types(self, scope: Optional[Scope] = None) -> list[tuple[K, T]]:
        return [(x, info.ty) for x, info in self.__vars.items()
                if scope is None or info.scope == scope]
    def hasVar(self, var: K):
        return var in self.__vars
    def copy(self) -> Symtab[K, T]:
        st = Symtab[K, T]()
        st.__vars.update(self.__vars)
        return st
    def mergeBack(self, st1: Symtab[K, T], st2: Symtab[K, T]):
        import common.symtab_merge as symtab_merge
        merged = symtab_merge.merge(st1, st2)
        self.__vars = merged
