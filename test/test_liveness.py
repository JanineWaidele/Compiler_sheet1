import assembly.types as asTypes
import common.genericCompiler as genCompiler
import shell
import common.utils as utils
import common.log as log
import assembly.tacPretty as tacPretty
import common.testsupport as testsupport
import pytest

pytestmark = pytest.mark.instructor

def buildInterfGraph(args: genCompiler.Args) -> asTypes.InterfGraph:
    # We have to import these modules dynamically because they are not present in student code
    controlFlow = utils.importModuleNotInStudent('compilers.assembly.controlFlow')
    liveness = utils.importModuleNotInStudent('compilers.assembly.liveness')
    asCommon = utils.importModuleNotInStudent('compilers.assembly.common')
    log.debug(f'Interference graph test, first compilin to TAC')
    tacInstrs = asCommon.loopToTac(args)
    log.debug(f'TAC:\n{tacPretty.prettyInstrs(tacInstrs)}')
    log.debug(f'Building the control flow graph ...')
    ctrlFlowG = controlFlow.buildControlFlowGraph(tacInstrs)
    log.debug(f'Control flow graph: {ctrlFlowG}')
    log.debug(f'Building the interference graph ...')
    interfGraph = liveness.buildInterfGraph(ctrlFlowG)
    log.debug(f'Interference graph: {interfGraph}')
    return interfGraph

def interfGraphTest(src: str, expectedConflicts: list[tuple[str, str]]):
    l: list[tuple[str, str]] = expectedConflicts[:]
    for (x, y) in expectedConflicts:
        l.append((y, x))
    expectedConflicts = l
    with shell.tempDir() as d:
        srcFile = shell.pjoin(d, "input.py")
        outFile = shell.pjoin(d, "out.wasm")
        utils.writeTextFile(srcFile, src)
        args = genCompiler.Args(srcFile, outFile)
        interfGraph = buildInterfGraph(args)
        realConflicts = [(x.name, y.name) for (x, y) in interfGraph.edges]
        for c in realConflicts:
            if c not in expectedConflicts:
                pytest.fail(f'Conflict {c} in interference graph was not expected. realConflicts={realConflicts}')
        for c in expectedConflicts:
            if c not in realConflicts:
                pytest.fail(f'Expected conflict {c} not in interference graph. realConflicts={realConflicts}')

src1 = """
n = input_int()
res = 1
c = n <= 0
while c:
    res = res * n
    n = n - 1
    c = n <= 0
print(res)
"""

src2 = """
n = input_int()
s = 0
i = 1
c = i < n
while c:
    t = i * i
    s = s + t
    i = i + 1
    c = i < n
print(s)
"""

src3 = """
a = input_int()
b = input_int()
c = b == 1
if c:
    print(a)
else:
    print(0)
"""

def test_interfGraph1():
    interfGraphTest(src1, [('$n', '$res'), ('$res', '$c'), ('$c', '$n')])

def test_interfGraph2():
    n = '$n'
    s = '$s'
    i = '$i'
    c = '$c'
    t = '$t'
    interfGraphTest(src2, [(c, n), (c, s), (c, i), (n, s), (n, i), (n, t), (i, s), (i, t), (s, t)])

def test_interfGraph3():
    interfGraphTest(src3, [('$a', '$b'), ('$a', '$c')])

@pytest.mark.parametrize("lang, srcFile",
                         testsupport.collectTestFiles(langOnly=['loop', 'var'], ignoreErrorFiles=True))
def test_interfGraph(lang: str, srcFile: str, tmp_path: str):
    outFile = shell.pjoin(tmp_path, "out.wasm")
    args = genCompiler.Args(srcFile, outFile)
    buildInterfGraph(args) # make sure it does not fail
