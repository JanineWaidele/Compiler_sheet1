from common.wasm import *
from common.compilerSupport import *
import common.utils as utils

class Errors:
    """
    Class dealing with errors.
    """
    arraySize = 'ArraySizeError'
    arrayIndexOutOfBounds = 'IndexError'
    allErrors = [arraySize, arrayIndexOutOfBounds]
    @staticmethod
    def data() -> list[WasmData]:
        """
        Get WasmData instructions for declaring the error messages.
        """
        start = 0
        res: list[WasmData] = []
        for e in Errors.allErrors:
            res.append(WasmData(start, e))
            start += len(e)
        return res
    @staticmethod
    def outputError(s: str) -> list[WasmInstr]:
        """
        Returns a list of Wasm instructions for outputting an error message.
        The error message must be one of Errors.allErrors.
        """
        start = 0
        for e in Errors.allErrors:
            if s == e:
                break
            else:
                start += len(e)
        else:
            utils.abort(f'Unknown error message: {s}')
        return [WasmInstrConst('i32', start),
                WasmInstrConst('i32', len(s)),
                WasmInstrCall(WasmId('$print_err'))]

class Globals:
    """
    Class giving access to the names of global variables.
    """
    freePtr = WasmId('$@free_ptr')
    @staticmethod
    def decls() -> list[WasmGlobal]:
        """
        Returns a list of Wasm global declarations.
        """
        errsLen = 0
        for e in Errors.allErrors:
            errsLen += len(e)
        offset = 100 # must be 4-byte aligned
        if errsLen > offset:
            utils.abort(f'Offset for free_ptr is {offset}, but error messages take {errsLen} bytes')
        return [WasmGlobal(Globals.freePtr, 'i32', True, [WasmInstrConst('i32', 100)])]

class Locals:
    """
    Class giving access to the names of temporary local variables.
    """
    tmp_i32 = WasmId('$@tmp_i32')
    tmp_i64 = WasmId('$@tmp_i64')
    @staticmethod
    def decls() -> list[tuple[WasmId, WasmValtype]]:
        """
        Returns a list of local variable declarations to be used in a function definition.
        """
        return [(Locals.tmp_i32, 'i32'),
                (Locals.tmp_i64, 'i64')]
