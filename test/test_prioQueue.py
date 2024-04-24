from common.prioQueue import *

def lessInt(x: int, y: int) -> bool:
    return x < y

def ignore_update(obj: object, pos: int):
    pass

def test_heapBuildExtractMax():
    h = Heap([4,3,5,1,2], lessInt, ignore_update)
    for i in range(5, 0, -1):
        assert h.extractMax() == i

def test_heapInsert():
    l: list[int] = []
    h = Heap(l, lessInt, ignore_update)
    for k in [4,3,5,1,2]:
        h.insert(k)
    for i in range(5, 0, -1):
        assert h.extractMax() == i

def test_heapSort():
    l = [4,3,5,1,2]
    h = Heap(l, lessInt, ignore_update)
    heapSort(h)
    for i in range(0, 5):
        assert l[i] == i + 1

def test_prioQueuePushPop():
    d = {'a': 4, 'b': 3, 'c':5, 'd':1, 'e':2}
    def less(x: str, y: str):
        return d[x] < d[y]
    q = PrioQueue(less)
    for k, _v in d.items():
        q.push(k)
    p = ['c', 'a', 'b', 'e', 'd']
    for i in range(0, len(p)):
        assert q.pop() == p[i]

def test_prioQueuePushPopIncreaseKey():
    d = {'a': 4, 'b': 3, 'c':5, 'd':1, 'e':2}
    def less(x: str, y: str):
        return d[x] < d[y]
    q = PrioQueue(less)
    for k, _v in d.items():
        q.push(k)
    d = {'a': 6, 'b': 4, 'c':5, 'd':3, 'e':2}
    q.incKey('a')
    q.incKey('a')
    q.incKey('b')
    q.incKey('d')
    q.incKey('d')
    p = ['a', 'c', 'b', 'd', 'e']
    for i in range(0, len(p)):
        assert q.pop() == p[i]

def test_prioQueueIncreaseKey():
    d = {'a': 4, 'b': 3, 'c':5, 'd':1, 'e':2}
    def less(x: str, y: str):
        return d[x] < d[y]
    q = PrioQueue(less)
    for k, v in d.items():
        q.push(k)
    for k, v in d.items():
        d[k] = v + 2
        q.incKey(k)
    for i in range(5, 0, -1):
        k = q.pop()
        assert d[k] == i + 2

