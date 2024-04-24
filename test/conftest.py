import pytest

def pytest_configure(config: pytest.Config):
    config.addinivalue_line(
        "markers", "instructor: mark a test to be run only in the instructor repo"
    )

def pytest_itemcollected(item: pytest.Item):
    """
    Add the instructor keyword to all tests marked with
    pytest.mark.instructor.
    Then, 'instructor' can be used with the -k commandline option.
    """
    if not item.own_markers:
        return
    found = False
    for m in item.own_markers:
        if m.name == 'instructor':
            found = True
    if found and not 'instructor' in item.extra_keyword_matches:
        print('adding kw')
        item.extra_keyword_matches.update('instructor')
