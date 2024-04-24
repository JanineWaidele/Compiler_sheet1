import shell
import common.testsupport as testsupport
import pytest
import common.log as log
import common.constants as constants

pytestmark = pytest.mark.instructor

def run(wasm: str, input: str|None) -> shell.RunResult:
    log.info(f'Running the program')
    cmd = ['timeout', '10s', 'bash', './wasm-support/run_iwasm', wasm]
    res = shell.run(cmd, onError='ignore', captureStdout=True, captureStderr=True, input=input)
    if res.exitcode != 0:
        res = shell.RunResult(res.stderr, res.stderr, constants.RUN_ERROR_EXIT_CODE)
    return res

def runTest(lang: str, srcFile: str, tmp: str, captureErr: bool, input: str|None, extraArgs: str|None) -> shell.RunResult:
    output = shell.pjoin(tmp, 'out.wasm')
    cmd = f'python src/main.py --lang={lang} compile --output={output}'
    if extraArgs:
        cmd = cmd + ' ' + extraArgs
    cmd = cmd + ' ' + srcFile
    log.info(f'Running command {cmd}')
    res = shell.run(cmd,
                    captureStderr=captureErr, captureStdout=False, onError='ignore')
    if captureErr and res.stderr:
        log.info(f'Output on stderr: {res.stderr}')
    if res.exitcode == 0:
        return run(shell.pjoin(tmp, 'out.wasm'), input)
    else:
        return res

@pytest.mark.parametrize("lang, srcFile", testsupport.collectTestFiles())
def test_compiler(lang: str, srcFile: str, tmp_path: str):
    testsupport.runFileTest(
        srcFile,
        lambda captureErr, input, extraArgs: \
            runTest(lang, srcFile, tmp_path, captureErr, input, extraArgs)
    )

