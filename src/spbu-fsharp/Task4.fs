namespace HomeWork4

open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees


module VectorData =

    /// This sub-structure is used to build a Binary Tree from a given array.
    // The constructed Binary Tree will be then used as a data storage for a sparse vector.
    type Vector<'value> =
        struct
            /// Initial vector data.
            val Data: array<'value option>
            /// Leftmost array index.
            val Left: int
            /// Rightmost array index.
            val Right: int

            new(data, left, right) =
                { Data = data
                  Left = left
                  Right = right }

            member this.CurrentLength = this.Right - this.Left + 1

            /// Maximum real available index.
            member this.DataMaxIndex = this.Data.Length - 1
        end


    /// Splits a given Vector in half.
    let vecPartition (vec: Vector<'value>) =
        let newRight = (vec.Left + vec.Right) / 2
        Vector(vec.Data, vec.Left, newRight), Vector(vec.Data, newRight + 1, vec.Right)


    let vecToTree (arr: array<'value option>) =
        let rec maker (vec: Vector<'value>) =
            // If we look at a vector which starting index is out of bounds,
            // then there is no real data in it, no need to store anything.
            if vec.Left > vec.DataMaxIndex then
                BinTree.None
            // If we find a cell that is within the bounds of the initial array,
            // then we look at the data in that cell and store it accordingly.
            elif (vec.CurrentLength = 1)
                 && (vec.Left <= vec.DataMaxIndex) then
                let data = vec.Data[vec.Left]

                match data with
                | Some value -> Leaf value
                | Option.None -> BinTree.None
            else
                // Otherwise we split the array (padded with indices to the power of 2) in two halves
                // and repeat the process.
                let leftPart, rightPart = vecPartition vec

                // To get rid of the unnecessary data and store identical data more efficiently
                // we merge a node into a single one based on the node's children.
                let result = Node(maker leftPart, maker rightPart)

                match result with
                | Node (BinTree.None, BinTree.None) -> BinTree.None
                | Node (Leaf value1, Leaf value2) when value1 = value2 -> Leaf value1
                | _ -> result

        // Construct a Vector type from a given array and pad it with the
        // maximum index that this array would have if its length were equal to a power of two.
        let paddedIndex = (Numbers.ceilPowTwo arr.Length) - 1

        let vec = Vector(arr, 0, paddedIndex)

        // Make a tree.
        maker vec


module SparseVector =

    type Direction =
        | Left // Left child of a node in a Binary Tree.
        | Right // Right child of a node in a Binary Tree.

    type SparseVector<'value> =
        val Length: int
        val Data: BinTree<'value>

        new(length, data) = { Data = data; Length = length }

        member this.GetValue i =

            let rec look (direction: Direction) (tree: BinTree<'value>) =
                match direction, tree with
                | Left, Node (leftChild, _) -> leftChild
                | Right, Node (_, rightChild) -> rightChild
                | _, Leaf value -> Leaf value
                | _, None -> None


            /// Search function traverses a binary tree and looks for a value at a given index.
            // Index lookup happens as if the tree was an array by using two pointers (left, right).
            let rec search ix left right tree =
                let middle = (left + right) / 2

                if ix <= middle then
                    let node = look Left tree

                    match node with
                    | Leaf value -> Some value
                    | None -> Option.None
                    | _ -> search ix left middle node
                else
                    let node = look Right tree

                    match node with
                    | Leaf value -> Some value
                    | None -> Option.None
                    | _ -> search ix (middle + 1) right node

            let getValue (i: int) =
                let ix = i - 1 // Vector indices (i) start at 1, so we offset by one.

                if ix < 0 || ix > this.Length - 1 then
                    failwith "Index out of bounds."
                else
                    search ix 0 (this.Length - 1) this.Data

            getValue i


    let toSparse (arr: 'value option array) =
        let length = arr.Length
        let data = VectorData.vecToTree arr
        SparseVector(length, data)
