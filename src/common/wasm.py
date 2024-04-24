from __future__ import annotations
from typing import *
from dataclasses import dataclass
from common.sexp import *

type WasmValtype = Literal['i32', 'i64', 'f32', 'f64']

def renderValtype(t: WasmValtype) -> SExp:
    return SExpId(t)

@dataclass(frozen=True)
class WasmId:
    id: str
    def __post_init__(self):
        if not self.id or self.id[0] != '$':
            raise ValueError(f'Invalid wasm identifier: {self.id}')
    def render(self) -> SExp:
        return SExpId(self.id)

@dataclass(frozen=True)
class WasmModule:
    """
    A whole module, e.g. (module ...)
    """
    imports: list[WasmImport]
    exports: list[WasmExport]
    globals: list[WasmGlobal]
    data: list[WasmData]
    funcTable: WasmFuncTable
    funcs: list[WasmFunc]
    def render(self):
        return mkNamedSeq('module',
                          *[i.render() for i in self.imports],
                          *[e.render() for e in self.exports],
                          *[g.render() for g in self.globals],
                          *[d.render() for d in self.data],
                          self.funcTable.render(),
                          *[f.render() for f in self.funcs])

@dataclass(frozen=True)
class WasmImport:
    """
    Import declaration, e.g. (import "env" "print" <desc>)
    """
    module: str
    name: str
    desc: WasmImportDesc
    def render(self):
        return mkNamedSeq('import', SExpStr(self.module), SExpStr(self.name), self.desc.render())

type WasmImportDesc = WasmImportMemory | WasmImportFunc

@dataclass(frozen=True)
class WasmImportMemory:
    """
    Memory import, e.g. (memory 1)
    """
    min: int
    max: int | None
    def render(self):
        args = [SExpNum(self.min)]
        if self.max is not None:
            args.append(SExpNum(self.max))
        return mkNamedSeq('memory', *args)

@dataclass(frozen=True)
class WasmImportFunc:
    """
    Import of a function, e.g. (func $print (param i32 i32)
    """
    id: WasmId
    params: list[WasmValtype]
    result: WasmValtype | None
    def render(self) -> SExp:
        params = mkNamedSeq('param').append([renderValtype(t) for t in self.params])
        if self.result:
            result = mkNamedSeq('result', renderValtype(self.result))
            return mkNamedSeq('func', self.id.render(), params, result)
        else:
            return mkNamedSeq('func', self.id.render(), params)

@dataclass(frozen=True)
class WasmExport:
    """
    An export declaration, e.g. (export "main" <desc>)
    """
    name: str
    desc: WasmExportDesc
    def render(self) -> SExp:
        return mkNamedSeq('export', SExpStr(self.name), self.desc.render())

type WasmExportDesc = WasmExportFunc

@dataclass(frozen=True)
class WasmExportFunc:
    """
    Export of a function, e.g. (func $foo)
    """
    id: WasmId
    def render(self) -> SExp:
        return mkNamedSeq('func', self.id.render())

@dataclass(frozen=True)
class WasmGlobal:
    id: WasmId
    ty: WasmValtype
    mutable: bool
    init: list[WasmInstr]
    def render(self) -> SExp:
        t = SExpId(self.ty) if not self.mutable else mkNamedSeq('mut', SExpId(self.ty))
        init = [i.render() for i in self.init]
        return mkNamedSeq('global', self.id.render(), t, *init)

@dataclass(frozen=True)
class WasmData:
    start: int
    content: str
    def render(self) -> SExp:
        return mkNamedSeq('data', SExpId(f'(i32.const {self.start})'), SExpStr(self.content))

@dataclass(frozen=True)
class WasmFuncTable:
    elems: list[WasmId]
    def render(self):
        ids = [i.render() for i in self.elems]
        return mkNamedSeq('table', SExpId('funcref'), mkNamedSeq('elem', *ids))

@dataclass(frozen=True)
class WasmFunc:
    """
    Function definition, e.g. (func $foo <params> <result> <locals> <instructions>)
    """
    id: WasmId
    params: list[tuple[WasmId, WasmValtype]]
    result: Optional[WasmValtype]
    locals: list[tuple[WasmId, WasmValtype]]
    instrs: list[WasmInstr]
    def render(self) -> SExp:
        params = [mkNamedSeq('param', i.render(), renderValtype(t)) for (i, t) in self.params]
        res = []
        if self.result:
            res = [mkNamedSeq('result', renderValtype(self.result))]
        locals = [mkNamedSeq('local', i.render(), renderValtype(t)) for (i, t) in self.locals]
        instrs = [x.render() for x in self.instrs]
        return mkNamedSeq('func', self.id.render()).append(params + res + locals + instrs)

@dataclass(frozen=True)
class WasmInstrConst:
    """
    Constant instructions, e.g. i32.const
    """
    ty: WasmValtype
    val: float | int
    def render(self) -> SExp:
        return mkSeq(SExpId(f'{self.ty}.const'), SExpNum(self.val))

@dataclass(frozen=True)
class WasmInstrDrop:
    def render(self) -> SExp:
        return SExpId('drop')

@dataclass(frozen=True)
class WasmInstrNumBinOp:
    """
    Binary operators on numbers, e.g. i32.add
    """
    ty: WasmValtype
    op: Literal['add', 'sub', 'mul', 'shr_u', 'shl', 'xor', 'and'] # i added 'and'
    def render(self) -> SExp:
        return SExpId(f'{self.ty}.{self.op}')

@dataclass(frozen=True)
class WasmInstrIntRelOp:
    ty: Literal['i32', 'i64']
    op: Literal['eq', 'ne', 'lt_s', 'lt_u', 'gt_s', 'gt_u', 'le_s', 'le_u', 'ge_s', 'ge_u']
    def render(self) -> SExp:
        return SExpId(f'{self.ty}.{self.op}')

@dataclass(frozen=True)
class WasmInstrConvOp:
    op: Literal['i32.wrap_i64', 'i64.extend_i32_u', 'i64.extend_i32_s']
    def render(self) -> SExp:
        return SExpId(self.op)

@dataclass(frozen=True)
class WasmInstrCall:
    """
    Function call instruction, e.g. call $foo
    """
    id: WasmId
    def render(self) -> SExp:
        return mkNamedSeq('call', self.id.render())

@dataclass(frozen=True)
class WasmInstrCallIndirect:
    """
    Indirect function call instruction, e.g. call_indirect (param i64) (result i64)
    """
    params: list[WasmValtype]
    result: Optional[WasmValtype]
    def render(self) -> SExp:
        tys = [mkNamedSeq('param', SExpId(t)) for t in self.params]
        if self.result:
            tys.append(mkNamedSeq('result', SExpId(self.result)))
        return mkNamedSeq('call_indirect', *tys)


@dataclass(frozen=True)
class WasmInstrVarLocal:
    """
    Reading and writing of local variables, e.g. local.get $foo
    """
    op: Literal['get', 'set', 'tee']
    id: WasmId
    def render(self) -> SExp:
        return mkSeq(SExpId(f'local.{self.op}'), self.id.render())

@dataclass(frozen=True)
class WasmInstrVarGlobal:
    """
    Reading and writing of local variables, e.g. local.get $foo
    """
    op: Literal['get', 'set']
    id: WasmId
    def render(self) -> SExp:
        return mkSeq(SExpId(f'global.{self.op}'), self.id.render())

@dataclass(frozen=True)
class WasmInstrMem:
    ty: WasmValtype
    op: Literal['load', 'store']
    def render(self) -> SExp:
        return SExpId(f'{self.ty}.{self.op}')

@dataclass(frozen=True)
class WasmInstrBranch:
    """
    branch instruction, either conditional br_if or unconditional br
    """
    target: WasmId
    conditional: bool
    def render(self) -> SExp:
        br = 'br_if' if self.conditional else 'br'
        return mkSeq(SExpId(br), self.target.render())

@dataclass(frozen=True)
class WasmInstrIf:
    """
    Conditional execution, e.g. if (result i32) ... else ... end
    """
    resultType: Optional[WasmValtype]
    thenInstrs: list[WasmInstr]
    elseInstrs: list[WasmInstr]
    def render(self) -> SExp:
        res = [] if self.resultType is None else \
            [SExpSeq([SExpId('result'), (SExpId(self.resultType))])]
        b1 = SExpBlockItem('if', res + [i.render() for i in self.thenInstrs])
        b2 = SExpBlockItem('else', [i.render() for i in self.elseInstrs])
        return SExpBlock([b1, b2])

@dataclass(frozen=True)
class WasmInstrLoop:
    """
    Loop construct, e.g. loop $label ...
    """
    label: WasmId
    body: list[WasmInstr]
    def render(self) -> SExp:
        return SExpBlock.singleItem('loop', [self.label.render()] + [i.render() for i in self.body])

@dataclass(frozen=True)
class WasmInstrBlock:
    """
    Block construct, e.g. block $label ...
    """
    label: WasmId
    result: Optional[WasmValtype]
    body: list[WasmInstr]
    def render(self) -> SExp:
        res = []
        if self.result:
            res = [mkNamedSeq('result', renderValtype(self.result))]
        return SExpBlock.singleItem('block',
                                    [self.label.render()] + res + [i.render() for i in self.body])

@dataclass(frozen=True)
class WasmInstrComment:
    text: str
    def render(self) -> SExp:
        return SExpId(f'(;{self.text};)')

@dataclass(frozen=True)
class WasmInstrTrap:
    def render(self) -> SExp:
        return SExpId('unreachable')


type WasmInstr = WasmInstrConst | WasmInstrNumBinOp | WasmInstrIntRelOp | WasmInstrConvOp \
               | WasmInstrCall | WasmInstrCallIndirect | WasmInstrVarLocal | WasmInstrVarGlobal \
               | WasmInstrBranch | WasmInstrIf | WasmInstrLoop | WasmInstrBlock | WasmInstrMem \
               | WasmInstrComment | WasmInstrTrap | WasmInstrDrop

# instructions used for loop and for compiling to assembly
type WasmInstrL = WasmInstrConst | WasmInstrNumBinOp | WasmInstrIntRelOp \
               | WasmInstrCall | WasmInstrVarLocal \
               | WasmInstrBranch | WasmInstrIf | WasmInstrLoop | WasmInstrBlock
