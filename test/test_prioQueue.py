from common.prioQueue import *

def lessInt(x: int, y: int) -> bool:
    return x < y

def ignore_update(obj: object, pos: int):
    pass

def test_heapBuildExtractMax():
    h = Heap([4,3,5,1,2])
    for i in range(5, 0, -1):
        assert h.extractMax() == i

def test_heapInsert():
    l: list[int] = []
    h = Heap(l)
    for k in [4,3,5,1,2]:
        h.insert(k, k)
    for i in range(5, 0, -1):
        assert h.extractMax() == i

def test_heapSort():
    l = [4,3,5,1,2]
    h = Heap(l)
    heapSort(h)
    assert h.data == [1,2,3,4,5]

def test_prioQueuePushPop():
    d = {'a': 4, 'b': 3, 'c':5, 'd':1, 'e':2}
    q = PrioQueue[str]()
    for k, v in d.items():
        q.push(k, v)
    p = ['c', 'a', 'b', 'e', 'd']
    for i in range(0, len(p)):
        assert q.pop() == p[i]

def test_prioQueuePushPopIncreaseKey():
    d = {'a': 4, 'b': 3, 'c':5, 'd':1, 'e':2}
    q = PrioQueue[str]()
    for k, v in d.items():
        q.push(k, v)
    q.incPrio('a')
    q.incPrio('a')
    q.incPrio('b')
    q.incPrio('d')
    q.incPrio('d')
    p = ['a', 'c', 'b', 'd', 'e']
    for i in range(0, len(p)):
        assert q.pop() == p[i]

def test_prioQueueIncreaseKey():
    d = {'a': 4, 'b': 3, 'c':5, 'd':1, 'e':2}
    q = PrioQueue[str]()
    for k, v in d.items():
        q.push(k, v)
    for k, _ in d.items():
        q.incPrio(k)
    l: list[str] = []
    for _ in range(5, 0, -1):
        k = q.pop()
        l.append(k)
    assert l == ['c', 'a', 'b', 'e', 'd']

