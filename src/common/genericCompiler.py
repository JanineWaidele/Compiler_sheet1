from __future__ import annotations
import common.genericParser as parser
import common.log as log
from typing import *
from dataclasses import dataclass
from common.wasm import *
import common.sexp as sexp
import common.utils as utils
from common.compilerSupport import CompilerConfig
import common.compilerSupport as compilerSupport
import shell

type CompileFun = Callable[[Any, CompilerConfig], WasmModule]

def compileToWat(compileFun: CompileFun, astMod: Any, cfg: CompilerConfig,
                 input: str, output: str) -> WasmModule:
    ast = parser.parseFile(input, astMod)
    log.info(f'Compiling AST with {compileFun}')
    try:
        wasmMod = compileFun(ast, cfg)
    except compilerSupport.CompileError as e:
        e.displayAndDie()
    code = sexp.renderSExp(wasmMod.render())
    utils.writeTextFile(output, code)
    log.info(f'Wrote textual representation of wasm to {output}')
    return wasmMod

def wat2wasm(wat2wasmCmd: str, input: str, output: str):
    cmd = [wat2wasmCmd, '--output=' + output, input]
    log.info(f'Converting textual format of wasm to binary format, cmd: {cmd}')
    res = shell.run(cmd, onError='ignore')
    if res.exitcode != 0:
        utils.abort(f'wat2wasm failed with exit code {res.exitcode}')
    log.info(f'Successfully converted wat to wasm')

@dataclass(frozen=True)
class Args:
    input: str
    output: str
    wat2wasm: str = 'wat2wasm'
    maxMemSize: Optional[int] = None
    maxArraySize: Optional[int] = None
    maxRegisters: Optional[int] = None

def compileMain(args: Args, compileFun: CompileFun, astMod: Any) -> WasmModule:
    output = args.output
    outputBase, outputExt = shell.splitExt(output)
    outputWat = outputBase + '.wat'
    if outputExt not in ['.wat', '.wasm', '.as']:
        utils.abort(f'Extension of output file must be .wat or .wasm or .as')
    cfg = CompilerConfig(maxMemSize=args.maxMemSize or CompilerConfig.defaultMaxMemSize,
                         maxArraySize=args.maxArraySize or CompilerConfig.defaultMaxArraySize)
    wasmMod = compileToWat(compileFun, astMod, cfg, args.input, outputWat)
    if outputExt == '.wat':
        return wasmMod
    outputBin = outputBase + '.wasm'
    wat2wasm(args.wat2wasm, outputWat, outputBin)
    return wasmMod


