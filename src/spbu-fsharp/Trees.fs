namespace Trees

open CLists

module NTrees =

    /// N-ary tree representation.
    type NTree<'Value> =
        | Leaf of value: 'Value
        | Node of parent: 'Value * children: CList<NTree<'Value>>

    /// General n-ary tree folding function.
    let rec fold folder acc (tree: NTree<'Value>) =
        let recurse = fold folder

        match tree with
        | Leaf value -> folder acc value
        | Node (value, children) -> CLists.fold recurse (folder acc value) children

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
