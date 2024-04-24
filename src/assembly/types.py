from typing import *
from dataclasses import dataclass
import assembly.tac_ast as tac
import assembly.tacSpill_ast as tacSpill
import assembly.tacPretty as tacPretty
from assembly.graph import Graph

@dataclass
class BasicBlock:
    instrs: list[tac.instr]
    index: int
    labels: list[str]
    @property
    def last(self) -> Optional[tac.instr]:
        if not self.instrs:
            return None
        else:
            return self.instrs[-1]
    def __repr__(self):
        instrs = tacPretty.prettyInstrs(self.instrs, True)
        return f'BasicBlock({self.index}, {self.labels}, {instrs})'


type ControlFlowGraph = Graph[int, BasicBlock]
type InterfGraph = Graph[tac.ident, None]

class RegisterMap(Protocol):
    def resolve(self, x: tac.ident) -> Optional[tacSpill.ident]:
        pass

class RegisterAllocMap(RegisterMap):
    def __init__(self, m: dict[tac.ident, int], maxRegisters: int):
        self._m = m
        self.maxRegisters = maxRegisters
    def __str__(self):
        d = {}
        for x, i in self._m.items():
            if i >= 0 and i < self.maxRegisters:
                d[x.name] = f'$s{i}'
        return f'RegisterAllocMap({d})'
    def resolve(self, x: tac.ident) -> Optional[tacSpill.ident]:
        """
        Returns the register for x if it lives in a register. Otherwise returns None.
        """
        i = self._m.get(x)
        if i is None:
            return None
        elif i < 0 or i >= self.maxRegisters:
            return None
        else:
            return tacSpill.Ident(f'$s{i}')

# type RegisterMap = dict[tac.ident, int]
