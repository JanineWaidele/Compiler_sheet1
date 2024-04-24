import shell
import common.testsupport as testsupport
import common.log as log
import pytest

def runTest(lang: str, srcFile: str, input: str|None):
    cmd = ['timeout', '10s', 'python', 'src/main.py', f'--lang={lang}', 'interp', srcFile]
    log.info(f'Running command {" ".join(cmd)}')
    res = shell.run(cmd, input=input, captureStdout=True, captureStderr=True, onError='ignore')
    return res

@pytest.mark.parametrize("lang, srcFile", testsupport.collectTestFiles())
def test_interp(lang: str, srcFile: str):
    testsupport.runFileTest(
        srcFile,
        lambda captureErr, input, _extraArgs: runTest(lang, srcFile, input),
        errorMode='lenient'
    )

