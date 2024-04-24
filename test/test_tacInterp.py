from common.constants import *
import pytest
import common.testsupport as testsupport
import common.log as log
import shell

pytestmark = pytest.mark.instructor

def params() -> list[tuple[str, str]]:
    l = testsupport.collectTestFiles(['test_files'], ['var', 'simple'])
    return l

def runTest(lang: str, srcFile: str, tmp: str, captureErr: bool, input: str|None, extraArgs: str|None) -> shell.RunResult:
    cmd = f'python src/main.py --lang={lang} tacInterp {srcFile}'
    log.info(f'Running command {cmd}')
    res = shell.run(cmd, captureStderr=captureErr, captureStdout=True, onError='ignore', input=input)
    return res

@pytest.mark.parametrize("lang, srcFile", params())
def test_tacInterp(lang: str, srcFile: str, tmp_path: str):
    testsupport.runFileTest(
        srcFile,
        lambda captureErr, input, extraArgs: \
            runTest(lang, srcFile, tmp_path, captureErr, input, extraArgs)
    )

