from typing import *

type PrioDict[T] = dict[T, int]

class PrioQueue[T]:
    """
    A priority queue for elements of type T.
    """
    def __init__(self, secondaryOrder: dict[T, int]={}):
        self.heap = Heap[T](secondaryOrder=secondaryOrder)

    def __repr__(self):
        return repr(self.heap)

    def push(self, key: T, prio: int=0):
        """
        Adds an element to the priority queue.
        """
        self.heap.insert(key, prio)

    def pop(self) -> T:
        """
        Removes an element with the highest priority from the priority queue.
        """
        return self.heap.extractMax()

    def incPrio(self, key: T, by: int=1):
        """
        Increase priority of the given key by the given amount. The amount must not be
        negative (priorities never decrease).
        """
        self.heap.incPrio(key, by)

    def isEmpty(self) -> bool:
        return self.heap.size == 0

class Heap[T]:
    def __init__(self, data: list[T]=[], prios: dict[T, int]={}, secondaryOrder: dict[T, int]={}):
        self.secondaryOrder = secondaryOrder
        self.data: list[T] = []
        self.indices: dict[T, int] = {}
        self.size = 0
        self.prios: PrioDict[T] = {}
        for x in data:
            if x in prios:
                p = prios[x]
            elif isinstance(x, int):
                # special case for tests
                p = x
            else:
                raise ValueError(f'cannot determine initial priority for {x}')
            self.insert(x, p)

    def __repr__(self):
        return repr(self.data[:self.size])

    def getPrio(self, x: T) -> tuple[int, int]:
        if x in self.prios:
            return (self.prios[x], self.secondaryOrder.get(x, 0))
        else:
            raise ValueError(f'cannot determine priority for {x}')

    def less(self, x: T, y: T):
        return self.getPrio(x) < self.getPrio(y)

    def maximum(self) -> T:
        return self.data[0]

    def insert(self, key: T, prio: int):
        if key in self.prios:
            raise ValueError(f'Key {key} already present in heap')
        if prio < 0:
            raise ValueError('negative priorities are not allowed')
        self.prios[key] = prio
        self.size += 1
        idx = self.size - 1
        if len(self.data) < self.size:
            self.data.append(key)
        else:
            self.data[idx] = key
        self.indices[key] = idx
        heapAdjustAfterPrioInc(self, idx)

    def incPrio(self, key: T, by: int):
        if by < 0:
            raise ValueError('priorities must not decrease')
        idx = self.indices[key]
        self.prios[key] += by
        heapAdjustAfterPrioInc(self, idx)

    def extractMax(self) -> T:
        assert self.size != 0
        max = self.data[0]
        last = self.data[self.size-1]
        # FIXME self.data[self.size-1] = None
        self.data[0] = last
        self.indices[last] = 0
        self.size -= 1
        maxHeapify(self, 0)
        return max

    def updateIdx(self, i: int):
        key = self.data[i]
        self.indices[key] = i

def left(i: int) -> int:
    return 2 * i + 1

def right(i: int) -> int:
    return 2 * (i + 1)

def parent(i: int) -> int:
    return (i-1) // 2

def swap[T](A: list[T], i: int, j: int):
    tmp = A[i]
    A[i] = A[j]
    A[j] = tmp

def heapAdjustAfterPrioInc[T](H: Heap[T], i: int):
    while i > 0 and H.less(H.data[parent(i)], H.data[i]):
        swap(H.data, i, parent(i))
        H.updateIdx(i)
        H.updateIdx(parent(i))
        i = parent(i)

def maxHeapify[T](H: Heap[T], i: int):
    l = left(i)
    r = right(i)
    if l < H.size and H.less(H.data[i], H.data[l]):
        largest = l
    else:
        largest = i
    if r < H.size and H.less(H.data[largest], H.data[r]):
        largest = r
    if largest != i:
        swap(H.data, i, largest)
        H.updateIdx(i)
        H.updateIdx(largest)
        maxHeapify(H, largest)

def buildMaxHeap[T](H: Heap[T]):
    H.size = len(H.data)
    last_parent = len(H.data) // 2
    for i in range(last_parent, -1, -1):
        maxHeapify(H, i)

def heapSort[T](H: Heap[T]):
    buildMaxHeap(H)
    for i in range(len(H.data)-1, 0, -1):
        swap(H.data, 0, i)
        H.updateIdx(0)
        H.updateIdx(i)
        H.size -= 1
        maxHeapify(H, 0)

