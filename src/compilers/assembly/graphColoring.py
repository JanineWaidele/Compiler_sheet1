from assembly.common import *
import assembly.tac_ast as tac
import common.log as log
from common.prioQueue import PrioQueue


def chooseColor(x: tac.ident, forbidden: dict[tac.ident, set[int]]) -> int:
    """
    Returns the lowest possible color for variable x that is not forbidden for x.
    """
    # 3. Find the lowest color c not in {COL[v]|v∈adj(u)}.
    # TODO: 
    if x in forbidden.keys():
        forb_x = forbidden[x]
        if len(forb_x) >= MAX_REGISTERS:
            raise IndexError("Not enough registers")
        
        for ic in range(MAX_REGISTERS):
            if ic not in forb_x:
                return ic
            
        raise IndexError("Not enough registers")
    
    return 0

def colorInterfGraph(g: InterfGraph, secondaryOrder: dict[tac.ident, int]={},
                     maxRegs: int=MAX_REGISTERS) -> RegisterMap:
    """
    Given an interference graph, computes a register map mapping a TAC variable
    to a TACspill variable. You have to implement the "simple graph coloring algorithm"
    from slide 58 here.

    - Parameter maxRegs is the maximum number of registers we are allowed to use.
    - Parameter secondaryOrder is used by the tests to get deterministic results even
      if two variables have the same number of forbidden colors.
    """
    colors: dict[tac.ident, int] = {}
    forbidden: dict[tac.ident, set[int]] = {}
    q = PrioQueue(secondaryOrder)

    # 1. set W to to the set of all vertices of g
    W: list[tac.ident] = list(g.vertices)
    for v in g.vertices:
        q.push(v)
    # # 2. Pick u from W with the largest set forbidden(u) (break ties randomly).
    for idx_v in range(len(W)):
        v = W[idx_v]
        if v in dict(g.edges).keys():
            edgs = dict(g.edges)[v]
            if edgs in forbidden.keys():
                forbidden[edgs].add(idx_v)
            else:
                forbidden[edgs] = set([idx_v])

    # choose colors for vertices
    while not q.isEmpty():
        vert = q.pop()
        chosenColor = chooseColor(vert,forbidden)
        # check that max number of registers available isn't exceeded
        if chosenColor >= maxRegs:
            continue
        else:
            colors[vert] = chosenColor
            for vs in g.succs(vert):
                if vs in forbidden.keys():
                    forbidden[vs].add(chosenColor)
                else:
                    forbidden[vs] = set([chosenColor])
    m = RegisterAllocMap(colors, maxRegs)
    log.debug(f"m: {m}")  
    return m
