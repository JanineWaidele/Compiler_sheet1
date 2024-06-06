from assembly.common import *
from assembly.graph import Graph
import assembly.tac_ast as tac
from assembly.tac_ast import *

def instrDef(instr: tac.instr) -> set[tac.ident]:
    """
    Returns the set of identifiers defined by some instrucution.
    """
    # TODO InputInt Call
    defSet: set[tac.ident] = set()
    match instr:
        case Assign(idn,_):
            defSet.add(idn)
        case Call(_,_,args):
            for ar in args:
                match ar:
                    case Name(nv):
                        defSet.add(nv)
                    case _:
                        pass
        case _: 
            pass
    return defSet


def instrUse(instr: tac.instr) -> set[tac.ident]:
    """
    Returns the set of identifiers used by some instrucution.
    """
    defSet: set[tac.ident] = set()
    match instr:
        case Call(cv,_,args):
            match cv:
                case Ident():
                    defSet.add(cv)
                case _:
                    pass
            for a in args:
                match a:
                    case Name(v):
                        defSet.add(v)
                    case Const():
                        pass
        case Assign(ai, lExp):
            defSet.add(ai)
            match lExp:
                case BinOp():
                    match lExp.left:
                        case Name(vn):
                            defSet.add(vn)
                        case Const():
                            pass
                    match lExp.right:
                        case Name(rn):
                            defSet.add(rn)
                        case Const():
                            pass
                case Prim(p):
                    match p:
                        case Name(pn):
                            defSet.add(pn)
                        case _:
                            pass
        case _:
            pass
    return defSet

# Each individual instruction has an identifier. This identifier is the tuple
# (index of basic block, index of instruction inside the basic block)
type InstrId = tuple[int, int]

class InterfGraphBuilder:
    def __init__(self):
        # self.before holds, for each instruction I, to set of variables live before I.
        self.before: dict[InstrId, set[tac.ident]] = {}
        # self.after holds, for each instruction I, to set of variables live after I.
        self.after: dict[InstrId, set[tac.ident]] = {}

    def __liveStart(self, bb: BasicBlock, s: set[tac.ident]) -> set[tac.ident]:
        """
        Given a set of variables s and a basic block bb, __liveStart computes
        the set of variables live at the beginning of bb, assuming that s
        are the variables live at the end of the block.

        Essentially, you have to implement the subalgorithm "Computing L_start" from
        slide 46 here. You should update self.after and self.before while traversing
        the instructions of the basic block in reverse.
        """
        Lbeforek: Set[tac.ident] = set()
        for idx_rev in range(len(bb.instrs)-1,-1,-1):

            inst = bb.instrs[idx_rev]
            insId: InstrId = (bb.index,idx_rev)
            # first round -> Lafterk is vars live at end of block     
            if idx_rev == len(bb.instrs)-1: 
                Lafterk = s
            else:
                # Lafterk is the successor which was calculated in prev round
                Lafterk = self.after[bb.index,idx_rev+1]   
            # Live before are vars that weren't just defined in this instruction
            # but include the ones that have been used
            Lbeforek = Lafterk.difference(instrDef(inst)).union(instrUse(inst))
            self.before[insId] = Lbeforek
            self.after[insId] = Lafterk
        # return the vars live at block beginning
        return Lbeforek


    def __liveness(self, g: ControlFlowGraph):
        """
        This method computes liveness information and fills the sets self.before and
        self.after.

        You have to implement the algorithm for computing liveness in a CFG from
        slide 46 here.
        """
        # Assume IN is a mapping from vertices to sets of variables.
        # 1.Initialize IN[B] =∅ for all verticesBof the CFG.
        IN: Dict[int,Set[tac.ident]] = {x: set() for x in list(g.vertices)}
        for b_idx in range(len(list(g.values))):

            bb = list(g.values)[b_idx]
            # TODO
            out = IN[b_idx]
            IN[b_idx] = self.__liveStart(bb, out)

                
            

    def __addEdgesForInstr(self, instrId: InstrId, instr: tac.instr, interfG: InterfGraph):
        """
        Given an instruction and its ID, adds the edges resulting from the instruction
        to the interference graph.

        You should implement the algorithm specified on the slide
        "Computing the interference graph" (slide 50) here.
        """
        cfg: ControlFlowGraph = Graph('undirected')
        self.__liveness(cfg)
        dSet = instrDef(instr)
        uSet = instrUse(instr)
        liveSet = dSet.union(uSet)
        for iden in liveSet:
            sa = self.after[instrId]
            for aIden in sa:
                if (iden in sa) & (iden != aIden):
                    interfG.addEdge(iden, aIden)


    def build(self, g: ControlFlowGraph) -> InterfGraph:
        """
        This method builds the interference graph. It performs three steps:

        - Use __liveness to fill the sets self.before and self.after.
        - Setup the interference graph as an undirected graph containing all variables
          defined or used by any instruction of any basic block. Initially, the
          graph does not have any edges.
        - Use __addEdgesForInstr to fill the edges of the interference graph.
        """
        # step 1
        self.__liveness(g) # fill sets before and after
        # step 2
        interG: InterfGraph = Graph('undirected')
        # step 3
        for entry in g.values:
            indx = 0
            for inst in entry.instrs:
                inId: InstrId = (entry.index,indx)
                self.__addEdgesForInstr(inId,inst,interG)
                indx += 1
        return interG

def buildInterfGraph(g: ControlFlowGraph) -> InterfGraph:
    builder = InterfGraphBuilder()
    return builder.build(g)
