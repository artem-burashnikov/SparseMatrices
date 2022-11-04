namespace HomeWork4

open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees
open Trees.QuadTrees


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

            member this.Length = this.Right - this.Left + 1

            /// Maximum real available index.
            member this.DataMaxIndex = this.Data.Length - 1
        end


    /// Splits a given Vector in half.
    let vecPartition (vec: Vector<'value>) =
        let newRight = (vec.Left + vec.Right) / 2
        Vector(vec.Data, vec.Left, newRight), Vector(vec.Data, newRight + 1, vec.Right)


    let getData =
        function
        | Some value -> BinTree.Leaf value
        | Option.None -> BinTree.None


    let vecToTree (arr: array<'value option>) =
        let rec maker (vec: Vector<'value>) =
            // If we find a cell that is within the bounds of the initial array,
            // then we look at the data in that cell and store it accordingly.
            // Special case for an array of a single element is needed,
            // since the resulting tree would contain unnecessary branch otherwise.
            if vec.Data.Length = 1
               || ((vec.Length = 1) && (vec.Left <= vec.DataMaxIndex)) then
                vec.Data[vec.Left] |> getData
            // If we look at a vector which starting index is out of bounds,
            // then there is no real data in it, no need to store anything.
            elif vec.Left > vec.DataMaxIndex then
                BinTree.None
            else
                // Otherwise we split the array (padded with indices to the power of 2) in two halves
                // and repeat the process.
                let leftPart, rightPart = vecPartition vec

                // To get rid of the unnecessary data and store identical data more efficiently
                // we merge a node into a single one based on the node's children.
                let result = BinTree.Node(maker leftPart, maker rightPart)

                match result with
                | BinTree.Node (BinTree.None, BinTree.None) -> BinTree.None
                | BinTree.Node (BinTree.Leaf value1, BinTree.Leaf value2) when value1 = value2 -> BinTree.Leaf value1
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

        member this.Item
            with get i =

                let rec look (direction: Direction) (tree: BinTree<'value>) =
                    match direction, tree with
                    | Left, BinTree.Node (leftChild, _) -> leftChild
                    | Right, BinTree.Node (_, rightChild) -> rightChild
                    | _, BinTree.Leaf value -> BinTree.Leaf value
                    | _, BinTree.None -> BinTree.None


                /// Search function traverses a binary tree and looks for a value at a given index.
                // Index lookup happens as if the tree was an array by using two pointers.
                let rec search ix left right tree =
                    let middle = (left + right) / 2

                    if ix <= middle then
                        let node = look Left tree

                        match node with
                        | BinTree.Leaf value -> Some value
                        | BinTree.None -> Option.None
                        | _ -> search ix left middle node
                    else
                        let node = look Right tree

                        match node with
                        | BinTree.Leaf value -> Some value
                        | BinTree.None -> Option.None
                        | _ -> search ix (middle + 1) right node

                let getValue (i: int) =
                    let ix = i - 1 // Vector indices (i) start at 1, so we offset by one.

                    if ix < 0 || ix > this.Length - 1 then
                        failwith "Index out of range."
                    else
                        search ix 0 (Numbers.ceilPowTwo this.Length - 1) this.Data

                getValue i


    let toSparse (arr: 'value option array) =
        let length = arr.Length
        let data = VectorData.vecToTree arr
        SparseVector(length, data)



module MatrixData =

    type Matrix<'value> =
        struct
            // Initial matrix data.
            val Data: array<array<'value option>>
            // Real number of columns.
            val DataCols: int
            // Real number of rows.
            val DataRows: int
            // Coordinates of the top-left cell.
            val Ix: int
            val Iy: int
            // Coordinates of the bottom-right cell.
            val Jx: int
            val Jy: int

            new(data, rows, cols, ix, iy, jx, jy) =
                { Data = data
                  DataRows = rows
                  DataCols = cols
                  Ix = ix
                  Iy = iy
                  Jx = jx
                  Jy = jy }

            member this.LengthX = this.Jy - this.Iy + 1

            member this.LengthY = this.Jx - this.Ix + 1
        end


    let mtxPartition (mtx: Matrix<'value>) =
        let newX = (mtx.Ix + mtx.Jx) / 2
        let newY = (mtx.Iy + mtx.Jy) / 2
        //
        //   |Ixy       |           |
        //   |    NW    |    NE     |
        //  _|__________|___________|_
        //   |          |           |
        //   |    SW    |    SE  Jxy|
        //
        Matrix(mtx.Data, mtx.DataRows, mtx.DataCols, mtx.Ix, mtx.Iy, newX, newY),  // NW
        Matrix(mtx.Data, mtx.DataRows, mtx.DataCols, mtx.Ix, newY + 1, newX, mtx.Jy),  // NE
        Matrix(mtx.Data, mtx.DataRows, mtx.DataCols, newX + 1, mtx.Iy, mtx.Jx, newY),  // SW
        Matrix(mtx.Data, mtx.DataRows, mtx.DataCols, newX + 1, newY + 1, mtx.Jx, mtx.Jy) // SE


    let getData =
        function
        | Some value -> QuadTree.Leaf value
        | Option.None -> QuadTree.None

    let identical a b c d =
        List.forall (fun x -> x = a) [ b; c; d ]

    let mtxToTree (table: array<array<'value option>>) =
        let rec maker (mtx: Matrix<'value>) =

            // If we find a cell within bounds of the original data, then store the cell's value accordingly.
            if mtx.LengthX = 1
               && mtx.LengthY = 1
               && mtx.Jx <= (mtx.DataRows - 1)
               && mtx.Jy <= (mtx.DataCols - 1) then

                mtx.Data[mtx.Ix][mtx.Iy] |> getData

            // If the Quadrant is fully out of range of the original data, no need to look further.
            // Otherwise we partition the Quadrant again.
            elif mtx.Iy > (mtx.DataCols - 1) || mtx.Ix > (mtx.DataRows - 1) then
                QuadTree.None
            else

                let nw, ne, sw, se = mtxPartition mtx

                let result = QuadTree.Node(maker nw, maker ne, maker sw, maker se)

                match result with
                | QuadTree.Node (QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.None) -> QuadTree.None
                | QuadTree.Node (QuadTree.Leaf value1, QuadTree.Leaf value2, QuadTree.Leaf value3, QuadTree.Leaf value4) when
                    identical value1 value2 value3 value4
                    ->
                    QuadTree.Leaf value1
                | _ -> result

        // Empty Matrix
        if table.Length = 0 then
            QuadTree.None
        // Matrix 1x1
        elif table.Length = 1 && table[0].Length = 1 then
            table[0][0] |> getData
        // Matrix m by n.
        elif table.Length > 0 && table[0].Length > 0 then
            let paddedIndex =
                Numbers.ceilPowTwo (max table.Length table[0].Length) - 1

            let mtx =
                Matrix(table, table.Length, table[0].Length, 0, 0, paddedIndex, paddedIndex)

            maker mtx
        else
            failwith "Incorrect table data."
