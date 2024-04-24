# AUTOMATICALLY GENERATED (2024-04-03 17:32:36)
from __future__ import annotations
from dataclasses import dataclass

type optional[T] = T | None

@dataclass(frozen=True)
class Ident:
    name: str

type ident = Ident
type string = str

@dataclass
class Add:
    pass

@dataclass
class Sub:
    pass

@dataclass
class Mul:
    pass

@dataclass
class Less:
    pass

@dataclass
class LessEq:
    pass

@dataclass
class Greater:
    pass

@dataclass
class GreaterEq:
    pass

@dataclass
class Eq:
    pass

@dataclass
class NotEq:
    pass

type op = Add | Sub | Mul | Less | LessEq | Greater | GreaterEq | Eq | NotEq

@dataclass
class AddI:
    pass

@dataclass
class LessI:
    pass

type opI = AddI | LessI

@dataclass
class Imm:
    value: int

type imm = Imm

@dataclass
class Reg:
    name: string

type reg = Reg

@dataclass
class Op:
    op: op
    target: reg
    left: reg
    right: reg

@dataclass
class OpI:
    opI: opI
    target: reg
    left: reg
    right: imm

@dataclass
class LoadWord:
    target: reg
    offset: imm
    src: reg

@dataclass
class LoadI:
    target: reg
    value: imm

@dataclass
class LoadA:
    target: reg
    label: str

@dataclass
class StoreWord:
    src: reg
    offset: imm
    baseAddr: reg

@dataclass
class BranchNeqZero:
    reg: reg
    label: string

@dataclass
class Branch:
    label: string

@dataclass
class Move:
    target: reg
    source: reg

@dataclass
class Syscall:
    pass

@dataclass
class Label:
    label: string

type instr = Op | OpI | LoadWord | LoadI | LoadA | StoreWord | BranchNeqZero | Branch | Move | Syscall | Label