from common.constants import *
import pytest
import common.utils as utils
from typing import Any

pytestmark = pytest.mark.instructor

def importModTinyJsonParser() -> Any:
    return utils.importModuleNotInStudent('parsers.tinyJson.tinyJson_parser')

type Json = str | int | dict[str, Json]

def parseTest(src: str, expected: Json):
    m = importModTinyJsonParser()
    res = m.parse(src)
    if res != expected:
        pytest.fail(f'Parsing {src} returned {res}, expected {expected}')

def test_simple():
    parseTest('1', 1)
    parseTest('"hello"', 'hello')

def test_object():
    parseTest('{}', {})
    parseTest('{"k1": 1}', {'k1': 1})
    parseTest('{"k1": "xy"}', {'k1': 'xy'})
    parseTest('{"k1": "xy", "k2": 42}', {'k1': 'xy', 'k2': 42})

def test_nestedObject():
    parseTest('{"k1": {}}', {'k1': {}})
    parseTest('{"k1": {"k1": 1, "k2": "foo"}}', {'k1': {'k1': 1, 'k2': 'foo'}})
