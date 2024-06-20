from assembly.common import *
import assembly.tac_ast as tac
import common.log as log
from common.prioQueue import PrioQueue

def addCurrentToForbidden(xi: tac.ident, curCol: int, forb: dict[tac.ident, set[int]], edges: dict[tac.ident,tac.ident])->dict[tac.ident, set[int]]:
    # k: y, v: x
    for (k, v) in edges.items():
        # x: x, v: x
        if xi == v:
            # add color of x to forb[y] 
            if k in forb.keys():
                forb[k].add(curCol)
            else:
                forb[k] = set([curCol])
    return forb


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
    for (elem, i) in secondaryOrder.items():
        q.push(elem,i)

    # 1. set W to to the set of all vertices of g
    W: list[tac.ident] = list(g.vertices)
    # 2. Pick u from W with the largest set forbidden(u) (break ties randomly).
    for idx_v in range(len(W)):
        v = W[idx_v]
        if v in dict(g.edges).keys():
            edgs = dict(g.edges)[v]
            if edgs in forbidden.keys():
                forbidden[edgs].add(idx_v)
            else:
                forbidden[edgs] = set([idx_v])

    # sort variables descending by forbidden length
    forbidden = dict(sorted(forbidden.items(), key=lambda item: len(item[1]), reverse=True))

    # choose colors for vertices
    while not q.isEmpty():
        vert = q.pop()
        chosenColor = chooseColor(vert,forbidden)
        # check that max number of registers available isn't exceeded
        if chosenColor >= maxRegs:
            continue
        else:
            colors[vert] = chosenColor
            forbidden = addCurrentToForbidden(vert, colors[vert],forbidden,dict(g.edges))
      
    m = RegisterAllocMap(colors, maxRegs)
    log.debug(f"m: {m}")  
    return m
