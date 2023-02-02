module Graphs

open SparseMatrix.SparseMatrix

type Vertex<'A> = 'A

type Edge<'A> = Vertex<'A> * Vertex<'A>

type Graph<'A, 'B when 'B: equality> =
    val vertices: seq<Vertex<'A>>
    val edges: seq<Edge<'A>>
    val adjMtx: SparseMatrix<'B>
