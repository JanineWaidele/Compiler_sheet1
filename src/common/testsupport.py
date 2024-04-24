import shell
from typing import *
import os
import common.utils as utils
import common.log as log
import threading
import common.constants as constants

_CACHE_DIR = '.test_cache'
_CACHE_LOCK = threading.Lock()

# If IGNORE_HASH is True, the golden file from .test_cache is considered as the only
# source if truth. This can be useful if you changed test cases but want to make sure
# that their output is still the same
IGNORE_HASH = False

def getGolden(srcFile: str, input: str|None):
    with _CACHE_LOCK:
        base = shell.removeExt(srcFile)
        srcMd5 = utils.md5(srcFile)
        cacheFile = shell.pjoin(_CACHE_DIR, base + '.golden')
        hashFile = shell.pjoin(_CACHE_DIR, base + '.hash')
        shell.mkdirs(shell.dirname(cacheFile))
        if IGNORE_HASH and shell.isFile(cacheFile):
            return utils.readTextFile(cacheFile).strip()
        if shell.isFile(cacheFile) and shell.isFile(hashFile):
            haveMd5 = utils.readTextFile(hashFile).strip()
            if srcMd5 == haveMd5:
                return utils.readTextFile(cacheFile).strip()
        # We do not have a cache file or it's out-of-date
        # Be careful to avoid race conditions
        cmd = ['timeout', '10s', 'python', 'src/main.py', 'pyrun', srcFile]
        log.info('Running command: {" ".join(cmd)}')
        res = shell.run(cmd, captureStdout=True, input=input, onError='ignore')
        if res.exitcode != 0:
            raise Exception(f'Running test file {srcFile} with python failed!')
        golden = res.stdout.strip()
        utils.writeTextFile(cacheFile, golden)
        utils.writeTextFile(hashFile, srcMd5)
        return golden

type ErrorKind = Literal['type error', 'run error']

def getExpectedError(srcFile: str) -> Optional[tuple[ErrorKind, str]]:
    src = utils.readTextFile(srcFile)
    lines = src.split('\n')
    prefix = '###'
    if lines and lines[0].startswith(prefix):
        line = lines[0][len(prefix):].strip()
        try:
            i = line.index(':')
            k = line[:i].strip()
            details = line[i+1:].strip()
        except ValueError:
            k = line
            details = None
        match k:
            case 'type error': return (k, details or 'type error')
            case 'run error': return (k, details or '')
            case _:
                utils.abort(f'Invalid test spec in {srcFile}: {lines[0]}')
    else:
        return None

def readFileOpt(path: str) -> str|None:
    if shell.isFile(path):
        return shell.readFile(path).strip()
    else:
        return None

# strict: error message must match the one provide in the input file
# lenient: error message must not match, but the test command must fail
# skip: tests for errors are skipped
type ErrorMode = Literal['strict', 'lenient']

def runFileTest(srcFile: str,
                run: Callable[[bool, str|None, str|None], shell.RunResult],
                errorMode: ErrorMode = 'strict'):
    if not shell.isFile(srcFile):
        raise Exception(f'Source file {srcFile} does not exist')
    base = shell.removeExt(srcFile)
    inFile = base + ".in"
    input = readFileOpt(inFile)
    extraArgs = readFileOpt(base + ".args")
    if extraArgs:
        extraArgs = extraArgs.strip()
    err = getExpectedError(srcFile)
    hasErr = err is not None
    log.info(f'Running test on {srcFile}')
    result = run(hasErr, input, extraArgs)
    log.info(f'Result: {result}')
    if not hasErr:
        assert result.exitcode == 0
        expectedOutput = getGolden(srcFile, input)
        realOutput = result.stdout.rstrip()
        assert expectedOutput == realOutput
    else:
        errKind, errDetails = err
        errDetails = errDetails.lower()
        expectedExitCode = constants.COMPILE_ERROR_EXIT_CODE if errKind == 'type error' \
            else constants.RUN_ERROR_EXIT_CODE
        lenientRunError = (errorMode == 'lenient')
        expectedExitCodes = [expectedExitCode] + ([0] if lenientRunError else [])
        assert result.exitcode in expectedExitCodes
        if not lenientRunError:
            realErr = result.stderr.strip().lower()
            assert errDetails in realErr

def collectTestFiles(baseDirs: list[str] = ['test_files'],
                     langOnly: Optional[list[str]] = None,
                     ignoreErrorFiles: bool = False) -> list[tuple[str, str]]:
    """
    Returns a list with tuples (lang, file).
    """
    langPrefix = 'lang_'
    testDict: dict[str, list[str]] = {}
    for base in baseDirs:
        for x in os.listdir(base):
            startDir = shell.pjoin(base, x)
            if x.startswith(langPrefix) and shell.isDir(startDir):
                lang = x[len(langPrefix):]
                for root, _dirs, files in os.walk(startDir):
                    for file in files:
                        if file.endswith(".py") and not shell.basename(file).startswith('.'):
                            utils.listDictAdd(testDict, lang, shell.pjoin(root, file))
    if not testDict:
        raise Exception("Could not collect any test files!")
    # We should run tests for simpler languages also for more advanced languages
    allLangs = constants.ALL_LANGUAGES
    for lang in testDict:
        if lang not in allLangs:
            raise Exception(f'Unknown language: {lang}')
    extraTests: dict[str, list[str]] = {}
    for i in range(len(constants.ALL_LANGUAGES)):
        lang = allLangs[i]
        simplerLangs = allLangs[:i]
        for l in simplerLangs:
            tests = testDict.get(l, [])
            utils.listDictAdd(extraTests, lang, tests)
    result: list[tuple[str, str]] = []
    for k, l in list(testDict.items()) + list(extraTests.items()):
        for v in l:
            result.append((k, v))
    if langOnly:
        filteredResult: list[tuple[str, str]] = []
        for k, v in result:
            if k in langOnly:
                filteredResult.append((k, v))
        result = filteredResult
    if ignoreErrorFiles:
        filteredResult: list[tuple[str, str]] = []
        for lang, file in result:
            if getExpectedError(file) is None:
                filteredResult.append((lang, file))
        result = filteredResult
    return result
