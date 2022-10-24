namespace HomeWork4


module SparseVector =

    type BinTree<'Value> =
        | None
        | Leaf of 'Value
        | Node of left: BinTree<'Value> * right: BinTree<'Value>



module SparseMatrix =

    type QuadTree<'Value> =
        | None
        | Leaf of 'Value
        | Node of NW: QuadTree<'Value> * NE: QuadTree<'Value> * SW: QuadTree<'Value> * SE: QuadTree<'Value>
