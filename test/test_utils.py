from common.utils import splitIf

def test_splitIf():
    l = [1, 2, 3, 4, 5, 6]
    assert splitIf(l, lambda x: x == 3) == ([1, 2], [3, 4, 5, 6])
    assert splitIf(l, lambda x: x == 3, 'left') == ([1, 2, 3], [4, 5, 6])
    assert splitIf(l, lambda x: x == 3, 'right') == ([1, 2], [3, 4, 5, 6])
    assert splitIf(l, lambda x: x == 7) == (l, [])
    assert splitIf(l, lambda x: x == 7, 'left') == (l, [])
    empty: list[int] = []
    assert splitIf(empty, lambda x: x == 3) == ([], [])
    assert splitIf(empty, lambda x: x == 3, 'left') == ([], [])
    assert splitIf([3], lambda x: x == 3) == ([], [3])
    assert splitIf([3], lambda x: x == 3, 'left') == ([3], [])
