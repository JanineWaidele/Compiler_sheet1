from typing import *

type Less[T] = Callable[[T, T], bool]
type UpdatePos[T] = Callable[[T, int], None]

class PrioQueue[T]:
    def __init__(self, lessT: Less[T]):
        less: Less[KeyWithPos[T]] = lambda x, y: lessT(x.key, y.key)
        self.heap = Heap[KeyWithPos[T]]([], less, updatePos)
        self.keyPos: dict[T, KeyWithPos[T]] = {}

    def __repr__(self):
        return repr(self.heap)

    def push(self, key: T):
        kp = KeyWithPos(key)
        self.keyPos[key] = kp
        self.heap.insert(kp)

    def pop(self) -> T:
        return self.heap.extractMax().key

    def incKey(self, key: T):
        obj = self.keyPos[key]
        heapIncKey(self.heap, obj.pos)

    def isEmpty(self) -> bool:
        return self.heap.size == 0

class KeyWithPos[T]:
    def __init__(self, k: T, pos: int=-1):
        self.key = k
        self.pos = pos

    def __repr__(self):
        return str(self.key) + '@' + repr(self.pos)

def updatePos[T](kp: KeyWithPos[T], pos: int):
    kp.pos = pos

class Heap[T]:
    def __init__(self, data: list[T], less: Less[T], update: UpdatePos[T]):
        self.data = data
        self.less = less
        self.update = update
        self.size = 0
        i = 0
        for obj in self.data:
            self.update(obj, i)
            i += 1
        buildMaxHeap(self)

    def __repr__(self):
        return repr(self.data[:self.size])

    def maximum(self):
        return self.data[0]

    def insert(self, obj: T):
        self.size += 1
        if len(self.data) < self.size:
            self.data.append(obj)
        else:
            self.data[self.size - 1] = obj
        self.update(obj, self.size - 1)
        heapIncKey(self, self.size - 1)

    def extractMax(self):
        assert self.size != 0
        max = self.data[0]
        self.data[0] = self.data[self.size-1]
        self.update(self.data[0], 0)
        self.size -= 1
        maxHeapify(self, 0)
        return max

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

def heapIncKey[T](H: Heap[T], i: int):
    while i > 0 and H.less(H.data[parent(i)], H.data[i]):
        swap(H.data, i, parent(i))
        H.update(H.data[i], i)
        H.update(H.data[parent(i)], parent(i))
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
        H.update(H.data[i], i)
        H.update(H.data[largest], largest)
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
        H.update(H.data[0], 0); H.update(H.data[i], i)
        H.size -= 1
        maxHeapify(H, 0)

