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

    /// Fold for BinTree types.
    let rec fold folder acc tree =
        let recurse = fold folder

        match tree with
        | BinTree.None -> folder acc Option.None
        | Leaf value -> folder acc (Option.Some value)
        | Node (left, right) ->
            let leftAcc = recurse acc left
            recurse leftAcc right

module QuadTrees =

    type QuadTree<'Value> =
        | None
        | Leaf of 'Value
        | Node of NW: QuadTree<'Value> * NE: QuadTree<'Value> * SW: QuadTree<'Value> * SE: QuadTree<'Value>

    /// Fold for QuadTree types.
    let rec fold folder acc tree =
        let recurse = fold folder

        match tree with
        | QuadTree.None -> folder acc QuadTree.None
        | Leaf value -> folder acc value
        | Node (NW, NE, SW, SE) ->
            let nwAcc = recurse acc NW
            let neAcc = recurse nwAcc NE
            let swAcc = recurse neAcc SW
            recurse swAcc SE
