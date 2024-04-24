from typing import *
import ast
import common.utils as utils
from common.utils import abort
import common.log as log
import pprint
import common.constants as constants
from common.constants import Language
import parsers.common as p
import dataclasses

# Display the AST of some python code:
# print(ast.dump(ast.parse('5 * [1]', mode='eval'), indent=4))    # or mode='exec'

def pp(x: Any):
    return ast.dump(x)

def unsupported(x: str):
    raise Exception(f'Parser does not support the following construct: {x}')

def transUnOp(op: ast.unaryop, m: Any) -> Any:
    match op:
        case ast.USub():
            return m.USub()
        case ast.Not():
            return m.Not()
        case _:
            unsupported(f'unary operator {pp(op)}')

def transBinOp(op: ast.operator, m: Any) -> Any:
    match op:
        case ast.Add(): return m.Add()
        case ast.Sub(): return m.Sub()
        case ast.Mult(): return m.Mul()
        case _:
            unsupported(f'binary operator {pp(op)}')

def transCompOp(op: ast.cmpop, m: Any) -> Any:
    match op:
        case ast.Eq(): return m.Eq()
        case ast.NotEq(): return m.NotEq()
        case ast.Lt(): return m.Less()
        case ast.LtE(): return m.LessEq()
        case ast.Gt(): return m.Greater()
        case ast.GtE(): return m.GreaterEq()
        case ast.Is(): return m.Is()
        case _:
            unsupported(f'comparison operator {pp(op)}')

def transBoolOp(op: ast.boolop, m: Any) -> Any:
    match op:
        case ast.And(): return m.And()
        case ast.Or(): return m.Or()
        case _:
            unsupported(f'bool operator {pp(op)}')

def transExp(e: ast.expr, m: Any, lang: Language) -> Any:
    # log.debug(f'Parsing {pp(e)}')
    match e:
        case ast.Constant(c):
            if type(c) is int:
                return m.IntConst(c)
            elif type(c) is bool:
                return m.BoolConst(c)
            elif type(c) is str:
                unsupported(f'string constant {repr(c)}')
            elif type(c) is float:
                unsupported(f'float constant {repr(c)}')
            else:
                unsupported(f'constant {c}')
        case ast.Name(v, _):
            return m.Name(m.Ident(v))
        case ast.Call(ast.Name(f, _), args, []) if lang != 'fun':
            fun = m.Ident(f)
            return m.Call(fun, [transExp(e, m, lang) for e in args])
        case ast.Call(exp, args, []) if lang == 'fun':
            fun = transExp(exp, m, lang)
            return m.Call(fun, [transExp(e, m, lang) for e in args])
        case ast.UnaryOp(op, e):
            return m.UnOp(transUnOp(op, m), transExp(e, m, lang))
        case ast.BinOp(size, ast.Mult(), ast.List(l)):
            match l:
                case [e]:
                    return m.ArrayInitDyn(transExp(size, m, lang), transExp(e, m, lang))
                case _:
                    unsupported(f'dynamic array initialization with not exactly one initial value')
        case ast.BinOp(left, op, right):
            return m.BinOp(transExp(left, m, lang), transBinOp(op, m), transExp(right, m, lang))
        case ast.IfExp(cond, thenExp, elseExp):
            return m.CondExp(transExp(cond, m, lang), transExp(thenExp, m, lang), transExp(elseExp, m, lang))
        case ast.Compare(left, [op], [right]):
            return m.BinOp(transExp(left, m, lang), transCompOp(op, m), transExp(right, m, lang))
        case ast.BoolOp(op, [left, right]):
            return m.BinOp(transExp(left, m, lang), transBoolOp(op, m), transExp(right, m, lang))
        case ast.List(es):
            return m.ArrayInitStatic([transExp(e, m, lang) for e in es])
        case ast.Subscript(e, idx):
            return m.Subscript(transExp(e, m, lang), transExp(idx, m, lang))
        case _:
            unsupported(f'expression {pp(e)}')

def transStmt(s: ast.stmt, m: Any, lang: Language) -> Any:
    match s:
        case ast.Assign([ast.Name(x)], e):
            return m.Assign(m.Ident(x), transExp(e, m, lang))
        case ast.Assign([ast.Subscript(leftExp, idx)], rightExp):
            return m.SubscriptAssign(transExp(leftExp, m, lang),
                                     transExp(idx, m, lang),
                                     transExp(rightExp, m, lang))
        case ast.Expr(e):
            return m.StmtExp(transExp(e, m, lang))
        case ast.If(cond, thenBody, elseBody):
            return m.IfStmt(transExp(cond, m, lang), transStmts(thenBody, m, lang), transStmts(elseBody, m, lang))
        case ast.While(cond, body, []):
            return m.WhileStmt(transExp(cond, m, lang), transStmts(body, m, lang))
        case ast.FunctionDef(name, ast.arguments([], args, None, [], [], None, []), body, [], ret):
            return m.FunDef(m.Ident(name), [transArg(a, m) for a in args],
                            transResultTy(ret, m), transStmts(body, m, lang))
        case ast.Return(e):
            if e is not None:
                return m.Return(transExp(e, m, lang))
            else:
                return m.Return(None)
        case _:
            unsupported(f'statement {pp(s)}')

def transArg(a: ast.arg, m: Any) -> Any:
    match a:
        case ast.arg(name, ty):
            return m.FunParam(m.Ident(name), transTy(ty, m))

def transResultTy(t: ast.expr | None, m: Any) -> Any:
    match t:
        case ast.Constant(None):
            return m.Void()
        case _:
            return m.NotVoid(transTy(t, m))

def transTy(t: ast.expr | None, m: Any) -> Any:
    match t:
        case ast.Constant(None):
            abort('None type not allowed here')
        case ast.Name(x, _):
            match x:
                case 'int': return m.Int()
                case 'bool': return m.Bool()
                case _: unsupported(f'type {pp(t)}')
        case ast.Subscript(ast.Name('list'), arg):
            return m.Array(transTy(arg, m))
        case ast.Subscript(ast.Name('Callable'), ast.Tuple([ast.List(args), res])):
            return m.Fun([transTy(a, m) for a in args], transResultTy(res, m))
        case _:
            unsupported(f'type {pp(t)}')

def transStmts(ss: Iterable[ast.stmt], m: Any, lang: Language) -> Any:
    match list(ss):
        case [ast.Pass()]: return []
        case l:
            return [transStmt(s, m, lang) for s in l]

def transModule(module: ast.mod, m: Any, lang: Language) -> Any:
    match module:
        case ast.Module(stmts, _):
            newStmts: list[Any] = []
            funDefs: list[Any] = []
            if lang == 'fun':
                for s in transStmts(stmts, m, lang):
                    match s:
                        case m.FunDef():
                            funDefs.append(s)
                        case _:
                            newStmts.append(s)
                return m.Module(funDefs, newStmts)
            else:
                return m.Module(transStmts(stmts, m, lang))
        case _:
            unsupported(f'construct at module level: {pp(module)}')

class ModWrapper:
    def __init__(self, mod: Any, lang: Language):
        self.mod = mod
        self.lang = lang
    def __getattr__(self, name: str) -> Any:
        try:
            return getattr(self.mod, name)
        except AttributeError:
            abort(f'Language {self.lang} does not support AST node {name}')

def parseFile(filename: str, m: Any) -> Any:
    log.info(f'Parsing {filename} with ast module {m}')
    modName: str = m.__name__
    l = utils.stripPrefix('lang_', modName[:modName.index('.')])
    lang = constants.asLanguage(l)
    with open(filename, 'r') as f:
        src = f.read()
        module = ast.parse(src, filename)
        w = ModWrapper(m, lang)
        x = transModule(module, w, lang)
        log.debug(f'AST: {pprint.pformat(x)}')
        return x

ParserArgs = p.ParserArgs

def parseWithOwnParser(filename: str, args: ParserArgs, astMod: Any,
                       parseFun: Callable[[p.ParserArgs], None]):
    code = utils.readTextFile(filename)
    args = dataclasses.replace(args, code=code)
    try:
        ownAst = parseFun(args)
    except p.ParseError as err:
        utils.abort(f'Parse error: {err}')
    print('AST:')
    print(ownAst)
    pyAst = parseFile(filename, astMod)
    if ownAst == pyAst:
        print('AST from owner parser matches the AST obtained via the python parser')
    else:
        print('ERROR: mismatch between AST from owner parser and AST from python parser')
