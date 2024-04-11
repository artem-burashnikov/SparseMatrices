namespace Trees

module BinTrees =

    type BinTree<'Value> =
        | None
        | Leaf of 'Value
        | Node of left: BinTree<'Value> * right: BinTree<'Value>


module QuadTrees =

    type QuadTree<'Value> =
        | None
        | Leaf of 'Value
        | Node of NW: QuadTree<'Value> * NE: QuadTree<'Value> * SW: QuadTree<'Value> * SE: QuadTree<'Value>
