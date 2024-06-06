from common.constants import *
import pytest
import common.testsupport as testsupport
import common.log as log
from common.utils import splitIf, readTextFile
import shell

pytestmark = pytest.mark.instructor

def params() -> list[tuple[str, str, int]]:
    l = testsupport.collectTestFiles(['test_files'], ['var', 'simple'], ignoreErrorFiles=True)
    maxRegisters = [8, 2, 1, 0]
    return [(lang, src, maxReg) for (lang, src) in l for maxReg in maxRegisters]

def checkMaxRegisters(asFile: str, maxRegisters: int):
    # We use the registers $s0, $s1 ... for use variables
    # and registers $t1, $t2, $t3 as temporary registers
    forbiddenRegisters = [f'$t{i}' for i in range(4, 10)] + \
        [f'$s{i}' for i in range(8) if i >= maxRegisters]
    code = readTextFile(asFile)
    for r in forbiddenRegisters:
        if r in code:
            raise ValueError(f'Assembler code uses forbidden register {r}. ' \
                f'Only {maxRegisters} are allowed.')

def runTest(lang: str, srcFile: str, maxRegisters: int,
            tmp: str, hasErr: bool, input: str|None, extraArgs: str|None) -> shell.RunResult:
    out = shell.mkTempFile('.as')
    cmd = f'python src/main.py --lang={lang} assembly --max-registers {maxRegisters} {srcFile} {out}'
    log.info(f'Running command {cmd}')
    res1 = shell.run(cmd, onError='ignore')
    if res1.exitcode != 0:
        return res1
    checkMaxRegisters(out, maxRegisters)
    res2 = shell.run(f'spim -file {out}', onError='ignore', input=input, captureStdout=True)
    if 'Address error' in res2.stdout and res2.exitcode == 0:
        res2.exitcode = 1
    lines: list[str] = [x.strip() for x in res2.stdout.strip().splitlines()]
    (headerLines, cleanLines) = \
        splitIf(lines,
                lambda x: x.startswith('Loaded:') and x.endswith('exceptions.s'),
                'left')
    if not headerLines:
        raise ValueError(f'Unexpected output from spim: {res2.stdout}')
    res2.stdout = '\n'.join(cleanLines)
    return res2

@pytest.mark.parametrize("lang, srcFile, maxRegisters", params())
def test_assembly(lang: str, srcFile: str, maxRegisters: int, tmp_path: str):
    testsupport.runFileTest(
        srcFile,
        lambda captureErr, input, extraArgs: \
            runTest(lang, srcFile, maxRegisters, tmp_path, captureErr, input, extraArgs)
    )

