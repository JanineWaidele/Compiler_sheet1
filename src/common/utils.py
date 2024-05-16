import sys
from typing import *
import hashlib
import importlib

def abort(msg: str) -> Never:
    sys.stderr.write(f'ERROR: {msg}\nAborting!')
    sys.exit(1)

def readTextFile(path: str) -> str:
    with open(path, 'r') as f:
        try:
            return f.read()
        except UnicodeDecodeError as err:
            err.add_note(f'Cannot decode content of file {path}')
            raise err

def writeTextFile(path: str, content: str):
    with open(path, 'w') as f:
        return f.write(content)

def inputInt(prompt: str) -> int:
    if sys.stdout.isatty():
        s = input(prompt)
    else:
        s = input()
    try:
        return int(s)
    except ValueError:
        raise ValueError('input read from stdin was not integer: {s}')

def assertType(x: Any, ty: type):
    if not type(x) is ty:
        raise ValueError(f'{x} should have type {ty} but has type {type(x)}')

def md5(path: str) -> str:
    hash_md5 = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()

def listDictAdd[K, V](d: dict[K, list[V]], k: K, v: V | list[V]):
    if not isinstance(v, list):
        listV = [v]
    else:
        listV: list[V] = v
    old = d.get(k)
    if old is None:
        d[k] = listV[:]
    else:
        d[k] = old + listV

def shorten(s: str, n: int):
    if len(s) < n:
        return s
    else:
        return s[:n] + '...'

def unzip[T, U](ls: list[tuple[T, U]]) -> tuple[list[T], list[U]]:
    xs: list[T] = []
    ys: list[U] = []
    for (x, y) in ls:
        xs += [x]
        ys += [y]
    return (xs, ys)

def flatten[T](ls: Iterable[list[T]]) -> list[T]:
    res: list[T] = []
    for x in ls:
        res.extend(x)
    return res

def stripPrefix(prefix: str, s: str) -> str:
    if s.startswith(prefix):
        return s[len(prefix):]
    else:
        raise ValueError(f'String {s} does not start with prefix {prefix}')

def assertNotNone[T](x: Optional[T]) -> T:
    if x is None:
        raise ValueError('Got unexpected None value')
    else:
        return x

def importModuleNotInStudent(modName: str) -> Any:
    try:
        m = importlib.import_module(modName)
        return m
    except ImportError as e:
        e.add_note(f'Could not import {modName}. Are in the student repo?')
        raise e

def splitIf[T](l: list[T], pred: Callable[[T], bool],
               includeMatch: Literal['left', 'right']='right') -> tuple[list[T], list[T]]:
    """
    splitIf(l, pred) splits the list l into two lists, such that the first list
    contains all elements of l up to the first element, but not including, for which
    pred is true.

    If the optional argument includeMatch is 'left', the first list will include the
    element for which pred is true.
    """
    for i in range(len(l)):
        if pred(l[i]):
            if includeMatch == 'left':
                i = i + 1
            return (l[:i], l[i:])
    return (l, [])
