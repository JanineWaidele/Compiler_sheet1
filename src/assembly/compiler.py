"""
This module defines a function that translates a file with L_loop source code to
MIPS assembly.
"""

from typing import *
from assembly.common import *
from common.compilerSupport import *
from assembly.tacToTacSpill import tacToTacSpill
from assembly.tacSpillToMips import tacSpillToMips
from assembly.tac_ast import *
import common.utils as utils
import common.log as log
import common.genericCompiler as genCompiler
import assembly.mipsPretty as mipsPretty
from assembly.loopToTac import loopToTac
import assembly.tacSpillPretty as tacSpillPretty

MIPS_START = """
  .data
newline:
  .asciiz  "\\n"
  .text
  .globl main
main:
"""

MIPS_END = """

  # exit
  li $v0,10
  syscall
"""

def compileFile(args: genCompiler.Args):
    log.info(f'Compiling {args.input} to assembly file {args.output}, args={args}')
    tacInstrs = loopToTac(args)
    log.debug('TAC:\n' + tacPretty.prettyInstrs(tacInstrs))
    maxRegs = args.maxRegisters if args.maxRegisters is not None else MAX_REGISTERS
    tacSpillInstrs = tacToTacSpill(tacInstrs, maxRegs)
    log.debug('TAC spill:\n' + tacSpillPretty.prettyInstrs(tacSpillInstrs))
    mipsInstrs = tacSpillToMips(tacSpillInstrs)
    s = mipsPretty.mipsPretty(mipsInstrs)
    utils.writeTextFile(args.output, MIPS_START + s + MIPS_END)
    log.info(f'Wrote assembly file {args.output}')

