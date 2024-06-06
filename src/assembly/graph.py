"""
This module implements a generatel purpose graph data structure.
"""

from typing import *

type GraphKind = Literal['directed', 'undirected']

class Graph[V, T]:
    """
    A graph with vertices of type V. This type must be hashable, it is usually an int or str.
    T is the data associated with V. It can be of arbitrary type.

    The graph can be directed or undirected, depending on the flag passed to the constructor.
    """
    def __init__(self, kind: GraphKind):
        self.kind = kind
        self.__vertexData: dict[V, T] = {}
        self.__edges: dict[V, set[V]] = {}
    def __repr__(self):
        return f'Graph(vertices={list(self.__vertexData.keys())}, edges={self.__edges})'
    def addVertex(self, v: V, x: T):
        """
        Adds a new vertex v, with associated data x.
        """
        if v in self.__vertexData:
            raise ValueError(f'Vertex {v} already added to graph')
        self.__vertexData[v] = x
    def hasVertex(self, v: V):
        return v in self.__vertexData
    def __assertVertex(self, v: V):
        if v not in self.__vertexData:
            raise ValueError(f'Unknown vertex: {v}')
    def addEdge(self, src: V, tgt: V):
        """
        Adds an edge between vertex src and vertex tgt.
        For an undirected, it adds two edges: one between src and tgt, the other
        between tgt and src.
        """
        self.__assertVertex(src)
        self.__assertVertex(tgt)
        self.__addEdge(src, tgt)
        if self.kind == 'undirected':
            self.__addEdge(tgt, src)
    def __addEdge(self, src: V, tgt: V):
        if src in self.__edges:
            self.__edges[src].add(tgt)
        else:
            self.__edges[src] = {tgt}
    def getData(self, v: V) -> T:
        """
        Returns the data associated with vertex v.
        """
        return self.__vertexData[v]
    @property
    def values(self) -> Iterable[T]:
        """
        Returns an iterable with all data associated with any vertex.
        """
        return self.__vertexData.values()
    @property
    def vertices(self) -> Iterable[V]:
        """
        Returns an iterable with vertices.
        """
        return self.__vertexData.keys()
    def succs(self, v: V) -> list[V]:
        """
        Given a vertex v, returns all vertices w such that there exists an edge
        from v to w.
        """
        if v in self.__edges:
            return list(self.__edges[v])
        else:
            return []
    @property
    def edges(self) -> list[tuple[V, V]]:
        """
        Returns all edges of the graph.
        """
        res: list[tuple[V, V]] = []
        for src, tgts in self.__edges.items():
            for tgt in tgts:
                res.append((src, tgt))
        return res
