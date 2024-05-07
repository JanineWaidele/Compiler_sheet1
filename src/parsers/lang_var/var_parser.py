from lark import ParseTree
from parsers.lang_simple.simple_ast import *
from parsers.common import *
import common.log as log
from lang_var.var_ast import *

grammarFile = "./src/parsers/lang_var/var_grammar.lark"

def parse(args: ParserArgs) -> exp:
    parseTree = parseAsTree(args, grammarFile, 'exp')
    ast = parseTreeToExpAst(parseTree)
    log.debug(f'AST: {ast}')
    return ast

def parseTreeToExpAst(t: ParseTree) -> exp:

    match t.data:
        case 'int_exp':
            return IntConst(int(asToken(t.children[0])))
        case 'variable_exp':
            return Name(Ident(str(asToken(t.children[0]))))
        case 'usub_exp':
            return UnOp(USub(), parseTreeToExpAst(asTree(t.children[0])))
        case 'comma_exp':
            return Call(Ident(str(asToken(t.children[0]))), [parseTreeToExpAst(asTree(t.children[1])),parseTreeToExpAst(asTree(t.children[3]))]) 
        case 'add_exp':
            e1, e2 = [asTree(c) for c in t.children]
            return BinOp(parseTreeToExpAst(e1), Add(), parseTreeToExpAst(e2))
        case 'mul_exp':
            e1, e2 = [asTree(c) for c in t.children]
            return BinOp(parseTreeToExpAst(e1), Mul(), parseTreeToExpAst(e2))
        case 'sub_exp':
            e1, e2 = [asTree(c) for c in t.children]
            return BinOp(parseTreeToExpAst(e1), Sub(), parseTreeToExpAst(e2))
        case 'sub_mult_exp':
            e1, e2, e3 = [asTree(c) for c in t.children]
            return BinOp(BinOp(parseTreeToExpAst(e1), Sub(), parseTreeToExpAst(e2)), Sub(), parseTreeToExpAst(e3))
        case 'exp_1' | 'exp_2' | 'exp_3' | 'paren_exp':
            return parseTreeToExpAst(asTree(t.children[0]))
        case 'exp_input':
            return Call(Ident(str(asToken(t.children[0]))), [])
        case 'exp_input_int':
            return Call(Ident(str(asToken(t.children[0]))), [IntConst(int(asToken(t.children[1])))])
        case 'exp_print_empty':
            return Call(Ident(str(asToken(t.children[0]))), [])
        case 'exp_print':
            return Call(Ident(str(asToken(t.children[0]))), [parseTreeToExpAst(asTree(t.children[1]))])
        case kind:
            raise Exception(f'unhandled parse tree of kind {kind} for exp: {t}')
    
def parseModule(args: ParserArgs) -> mod:
    parseTree = parseAsTree(args, grammarFile, 'lvar')
    return parseTreeToModuleAst(parseTree)

def parseTreeToStmtAst(t: ParseTree) -> stmt:
    match t.data:
        case 'exp_stmt':
            return StmtExp(parseTreeToExpAst(asTree(t.children[0])))
        case 'exp_assign':
            return Assign(Ident(str(asToken(t.children[0]))), parseTreeToExpAst(asTree(t.children[1])))
        case 'stmt_newline':
            return parseTreeToStmtAst(asTree(t.children[0]))
        case kind:
            raise Exception(f'unhandled parse tree of kind {kind} for stmt: {t}')

def parseTreeToStmtListAst(t: ParseTree) -> list[stmt]:
    return [parseTreeToStmtAst(asTree(c)) for c in t.children]

def parseTreeToModuleAst(t: ParseTree) -> mod:
    return Module(parseTreeToStmtListAst(t))