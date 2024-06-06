"""
This module defines a function `loopToTac` that translates to L_loop program
from a file to TAC.
"""
from typing import *
from assembly.common import *
from common.compilerSupport import *
import assembly.wasmToTac as wasmToTac
from assembly.tac_ast import *
import common.log as log
import common.genericCompiler as genCompiler
import assembly.wasmToTac as wasmToTac
import common.sexp as sexp
import common.utils as utils

def loopToTac(args: genCompiler.Args) -> list[tac.instr]:
    c = utils.importModuleNotInStudent('compilers.lang_loop.loop_compiler')
    import lang_loop.loop_ast as ast
    log.debug(f'Generating TAC from {args.input}')
    wasmMod = genCompiler.compileMain(args, c.compileModule, ast)
    wasmInstrs = wasmMod.funcs[0].instrs
    wasmCode = sexp.renderSExp(wasmMod.render())
    log.debug('Wasm instructions:\n' + wasmCode)
    (res, tacInstrs) = wasmToTac.wasmToTac(wasmToTac.downcast(wasmInstrs))
    if res is not None:
        raise ValueError(f'Value returned from tac.toTac is not None: {res}')
    return tacInstrs
