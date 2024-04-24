# AUTOMATICALLY GENERATED (2024-03-13 17:58:48)
from __future__ import annotations
from dataclasses import dataclass

type optional[T] = T | None

@dataclass(frozen=True)
class Ident:
    name: str

type ident = Ident
type string = str

@dataclass
class Op:
    name: string

type op = Op

@dataclass
class Const:
    value: int

@dataclass
class Name:
    var: ident

type prim = Const | Name

@dataclass
class Prim:
    p: prim

@dataclass
class BinOp:
    left: prim
    op: op
    right: prim

type exp = Prim | BinOp

@dataclass
class Assign:
    var: ident
    left: exp

@dataclass
class Call:
    var: optional[ident]
    name: ident
    args: list[prim]

@dataclass
class GotoIf:
    test: prim
    label: string

@dataclass
class Goto:
    label: string

@dataclass
class Label:
    label: string

type instr = Assign | Call | GotoIf | Goto | Label