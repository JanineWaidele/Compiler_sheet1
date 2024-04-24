import pytest
import common.log as log
import logging

# we want to have pytest assert introspection in the helpers
pytest.register_assert_rewrite('common.testsupport')

def _logLevel():
    import sys
    argv = sys.argv
    n = len(argv)
    for i in range(n):
        if argv[i] == '--log-level' and i + 1 < n:
            return log.resolveLevelName(sys.argv[i + 1])
    return logging.WARNING

log.init(_logLevel(), 'minipy_tests.log')
