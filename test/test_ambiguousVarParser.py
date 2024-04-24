import parsers.common as p
from common.constants import *
import pytest
import common.utils as utils

pytestmark = pytest.mark.instructor

def parseModule(args: p.ParserArgs):
    varParser = utils.importModuleNotInStudent('parsers.lang_var.var_parser')
    return varParser.parseModule(args)

def test_ambiguous():
    code = 'print(1-2-3)'
    args = p.ParserArgs(code, 'earley', None, 'src/parsers/lang_var/var_grammar_ambiguous.lark')
    with pytest.raises(p.ParseError) as err:
        parseModule(args)
    assert 'multiple parse trees' in str(err)

def test_shiftReduceConflict():
    code = 'print(1)'
    args = p.ParserArgs(code, 'lalr', None, 'src/parsers/lang_var/var_grammar_lalr-conflict.lark')
    with pytest.raises(p.ParseError) as err:
        parseModule(args)
    assert 'Shift/Reduce conflict for terminal NEWLINE' in str(err)
