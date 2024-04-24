from assembly.types import InterfGraph
from assembly.graph import Graph
import assembly.tac_ast as tac
import assembly.tacSpill_ast as tacSpill
import common.utils as utils
import pytest

pytestmark = pytest.mark.instructor

def regAllocTester(vars: list[str],
                   deps: list[tuple[str, str]],
                   expectedRegs: list[tuple[str, str]],
                   maxRegs: int=4):
    # We have to import this module dynamically because it is not present in student code
    regAlloc = utils.importModuleNotInStudent('compilers.assembly.regAlloc')
    g: InterfGraph = Graph('undirected')
    for x in vars:
        g.addVertex(tac.Ident(x), None)
    for x,y in deps:
        g.addEdge(tac.Ident(x), tac.Ident(y))
    secondaryOrder = dict([(tac.Ident(x), i) for i, x in enumerate(reversed(vars))])
    rm = regAlloc.colorInterfGraph(g, secondaryOrder, maxRegs)
    for x,r in expectedRegs:
        assert rm.resolve(tac.Ident(x)) == tacSpill.Ident(r)

def test_regAllocNoConflict():
    regAllocTester(['x', 'y'], [], [('x', '$s0'), ('y', '$s0')])

def test_regAllocConflict():
    regAllocTester(['x', 'y'], [('x', 'y')], [('x', '$s0'), ('y', '$s1')])

def test_regAllocManyVarsNoConflict():
    regAllocTester(['x', 'y', 'z'], [], [('x', '$s0'), ('y', '$s0'), ('z', '$s0')], maxRegs=2)

def test_regAllocManyVarsConflict():
    regAllocTester(['x', 'y', 'z'], [('x', 'y'), ('y', 'z')],
                   [('x', '$s0'), ('y', '$s1'), ('z', '$s0')], maxRegs=2)

def test_regAllocTooManyVarsConflict():
    regAllocTester(['x', 'y', 'z'], [('x', 'y'), ('y', 'z'), ('x', 'z')],
                   [('x', '$s0'), ('y', '$s1')], maxRegs=2)
