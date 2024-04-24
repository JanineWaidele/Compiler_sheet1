from typing import *
from lark import Lark, Token, Tree, ParseTree, tree, exceptions
import common.log as log
import common.utils as utils
from dataclasses import dataclass
import os

type ParseAlg = Literal['earley', 'lalr']

class TokenStream:
    """
    Essentially an iterator with lookahead functionality.
    """
    eof = Token('$END', '')
    def __init__(self, tokens: Iterable[Token]):
        self.tokenIter = iter(tokens)
        self._lookahead = None
    def next(self) -> Token:
        """
        Returns the next token, thereby consume the token.
        """
        if self._lookahead is not None:
            x = self._lookahead
            self._lookahead = None
            return x
        try:
            return next(self.tokenIter)
        except StopIteration:
            return TokenStream.eof
    def lookahead(self):
        """
        Returns the next token without consuming it.
        """
        if self._lookahead is None:
            self._lookahead = self.next()
        return self._lookahead
    def ensureNext(self, tokenType: str) -> Token:
        """
        Consumes the next token, ensuring that it has the given type.
        """
        t = self.next()
        if t.type != tokenType:
            raise ParseError(f'Expected token {tokenType} got {t} (type: {t.type})')
        else:
            return t
    def ensureEof(self, code: str):
        """
        Ensures that the next token is end-of-file.
        """
        t = self.lookahead()
        if t != TokenStream.eof:
            raise ParseError(f'Parsing {code} did not consume all tokens. ' \
                f'Tokens left: {[t] + self._list()}')
    def _list(self) -> list[Token]:
        l: list[Token] = []
        if self._lookahead:
            l.append(self._lookahead)
        return l + list(self.tokenIter)

def asToken(x: Token | ParseTree) -> Token:
    match x:
        case Token(): return x
        case _:
            raise ValueError(f'Expected single token but got a parse tree: {repr(x)}')

def asTree(x: Token | ParseTree) -> ParseTree:
    match x:
        case Token(): raise ValueError(f'Expected parse tree but got a single token: {repr(x)}')
        case _: return x

def removeNewlines(t: ParseTree):
    """
    Destructively removes all NEWLINE tokens from t.
    """
    children: list[Token | ParseTree] = []
    for c in t.children:
        match c:
            case Token('NEWLINE'): pass
            case Token():
                children.append(c)
            case _:
                removeNewlines(c)
                children.append(c)
    t.children = children

def isAmbiguous(t: ParseTree) -> bool:
    if t.data == '_ambig':
        return True
    else:
        for c in t.children:
            if isinstance(c, Tree) and isAmbiguous(c):
                return True
        return False

def parseTreeToPng(path: str, parseTree: ParseTree):
    pathDot = os.path.splitext(path)[0] + '.dot'
    try:
        tree.pydot__tree_to_dot(parseTree, pathDot) # type: ignore
        tree.pydot__tree_to_png(parseTree, path) # type: ignore
        log.debug(f'Stored visualization of parse tree in {path}')
    except Exception as err:
        log.debug(f'Could not render parse tree as png: {err}')

@dataclass(frozen=True)
class ParserArgs:
    code: str
    parseAlg: ParseAlg
    parseTreePng: Optional[str]
    grammarFile: Optional[str]

class ParseError(Exception):
    def __init__(self, msg: str):
        super().__init__(msg)

def mkLexer(grammarFile: str) -> Lark:
    return mkParser('earley', grammarFile, 'start')

def mkParser(alg: ParseAlg, grammarFile: str, start: str) -> Lark:
    grammar = utils.readTextFile(grammarFile)
    try:
        match alg:
            case 'earley':
                return Lark(grammar, start=start, ambiguity='explicit', parser='earley',
                            lexer='basic', debug=True)
            case 'lalr':
                return Lark(grammar, start=start, parser='lalr', strict=True,
                            debug=True, lexer='basic')
    except exceptions.LarkError as err:
        raise ParseError(f'Error constructing {alg} parser from grammar in {grammarFile}: {err}')

def _parseAsParseTree(parser: Lark, s: str, png: Optional[str]) -> ParseTree:
    s = s.rstrip() + '\n' # ensure there is one trailing newline
    try:
        lexed = parser.lex(s)
        lexedRepr = ['  ' + repr(tok) for tok in lexed]
        log.debug('tokens:\n' + '\n'.join(lexedRepr))
        parseTree = parser.parse(s)
    except exceptions.LarkError as err:
        raise ParseError(str(err))
    removeNewlines(parseTree)
    log.debug(f'parse tree:\n{parseTree}')
    log.debug(f'parse tree (pretty):\n{parseTree.pretty()}')
    if png is not None:
        parseTreeToPng(png, parseTree)
    if isAmbiguous(parseTree):
        raise ParseError(f'Got multiple parse trees (see logfile or png). ' \
            'You need to disambiguate your grammer.')
    return parseTree

def parseAsTree(args: ParserArgs, defaultGrammarFile: str, startSym: str) -> ParseTree:
    if args.grammarFile is None:
        grammarFile = defaultGrammarFile
    else:
        grammarFile = args.grammarFile
    parser = mkParser(args.parseAlg, grammarFile, startSym)
    parseTree = _parseAsParseTree(parser, args.code, args.parseTreePng)
    return parseTree

def unexpectedToken(t: Token, expected: str) -> Never:
    raise ParseError(f'Unexpected token {t} (token type: {t.type}). Expected: {expected}')
