namespace HomeWork4

open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees
open Trees.QuadTrees


module VectorData =

    /// This sub-structure is used to build a Binary Tree from a given array.
    // The constructed Binary Tree will be then used as a data storage for a sparse vector.
    type Vector<'Value>(arr: 'Value option array, head: int, length: int) =
        struct
            member this.Memory = arr
            member this.Head = head
            member this.Length = length
            member this.DataMaxIndex = arr.Length - 1
        end



    /// Splits a given Vector in half.
    let vecDiv2 (vec: Vector<'Value>) =
        let newLength = vec.Length / 2
        Vector(vec.Memory, vec.Head, newLength), Vector(vec.Memory, vec.Head + newLength, newLength)



    let reduce binTreeNode =
        match binTreeNode with
        | BinTree.Node(BinTree.None, BinTree.None) -> BinTree.None
        | BinTree.Node(BinTree.Leaf a1, BinTree.Leaf a2) when a1 = a2 -> BinTree.Leaf a1
        | _ -> binTreeNode



    let getData =
        function
        | Some value -> BinTree.Leaf value
        | Option.None -> BinTree.None



    let vecToTree (arr: array<'Value option>) =
        let rec maker (vec: Vector<'Value>) =
            if vec.Head > vec.DataMaxIndex then
                BinTree.None
            else if vec.Length = 1 then
                vec.Memory[vec.Head] |> getData
            else
                let leftPart, rightPart = vecDiv2 vec

                BinTree.Node(maker leftPart, maker rightPart) |> reduce

        // Check a given vector's length and act accordingly.
        let vec = Vector(arr, 0, arr.Length)

        if vec.Length = 0 then
            BinTree.None
        elif (vec.Length = 1) then
            vec.Memory[vec.Head] |> getData
        else
            let powerLength = Numbers.ceilPowTwo arr.Length

            let vec = Vector(arr, 0, powerLength)
            maker vec



open VectorData

module SparseVector =

    type SparseVector<'Value when 'Value: equality>(arr: 'Value option array) =
        let data = VectorData.vecToTree arr
        member this.Length = arr.Length

        member this.Data = data

        member this.Item
            with get i =

                /// Search function traverses a binary tree and looks for a value at a given index.
                // Index lookup happens as if the tree was an array by using two local pointers.
                let rec search i (left, right) tree =
                    match tree with
                    | BinTree.Leaf value -> Some value
                    | BinTree.None -> Option.None
                    | BinTree.Node(leftChild, rightChild) ->
                        let middle = (left + right) / 2

                        if i <= middle then
                            search i (left, middle) leftChild
                        else
                            search i (middle + 1, right) rightChild


                let getValue (i: int) =
                    let ix = i - 1 // Vector's index (i) starts at 1, so we offset by one.

                    if ix < 0 || ix > this.Length - 1 then
                        failwith $"SparseVector.Item with get(i): Index %A{ix} is out of range."
                    else
                        search ix (0, Numbers.ceilPowTwo this.Length - 1) this.Data

                getValue i



open SparseVector

module MatrixData =

    type Matrix<'Value>(arr2d: 'Value option[,], x: int, y: int, rows: int, columns: int) =
        struct
            member this.Memory = arr2d
            member this.HeadX = x
            member this.HeadY = y
            member this.Rows = rows
            member this.Columns = columns

            member this.MaxRowIndex = (Array2D.length1 arr2d) - 1

            member this.MaxColumnIndex = (Array2D.length2 arr2d) - 1
        end



    let mtxPartition (mtx: Matrix<'Value>) =
        if mtx.Rows = 0 || mtx.Columns = 0 then
            mtx, mtx, mtx, mtx
        else
            let halveRows = mtx.Rows / 2
            let halveColumns = mtx.Columns / 2
            //
            //   |          |           |
            //   |    NW    |    NE     |
            //  _|__________|___________|_
            //   |          |           |
            //   |    SW    |    SE     |
            //
            Matrix(mtx.Memory, mtx.HeadX, mtx.HeadY, halveRows, halveColumns), // NW
            Matrix(mtx.Memory, mtx.HeadX, mtx.HeadY + halveColumns, halveRows, halveColumns), // NE
            Matrix(mtx.Memory, mtx.HeadX + halveRows, mtx.HeadY, halveRows, halveColumns), // SW
            Matrix(mtx.Memory, mtx.HeadX + halveRows, mtx.HeadY + halveColumns, halveRows, halveColumns) // SE



    let reduce quadTreeNode =
        match quadTreeNode with
        | QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.None) -> QuadTree.None
        | QuadTree.Node(QuadTree.Leaf nw, QuadTree.Leaf ne, QuadTree.Leaf sw, QuadTree.Leaf se) when
            nw = ne && nw = sw && nw = se
            ->
            QuadTree.Leaf nw
        | _ -> quadTreeNode



    let getData =
        function
        | Some value -> QuadTree.Leaf value
        | Option.None -> QuadTree.None



    /// This function coverts a given table into a QudTree by splitting data into 4 quadrants.
    let tableToTree (table: 'Value option[,]) =
        let rec maker (mtx: Matrix<'Value>) =
            // If we find a cell within bounds of the original data, then store the cell's value accordingly.
            if
                mtx.Rows = 1
                && mtx.Columns = 1
                && mtx.HeadX <= mtx.MaxRowIndex
                && mtx.HeadY <= mtx.MaxColumnIndex

            then
                mtx.Memory[mtx.HeadX, mtx.HeadY] |> getData

            // If the Quadrant is fully out of range of the original data, no need to look further.
            // Otherwise we partition the Quadrant again.
            elif mtx.HeadX > mtx.MaxRowIndex || mtx.HeadY > mtx.MaxColumnIndex then
                QuadTree.None
            else

                let nw, ne, sw, se = mtxPartition mtx

                QuadTree.Node(maker nw, maker ne, maker sw, maker se) |> reduce

        // Check a given table's dimensions and act accordingly.
        let rows, columns = Array2D.length1 table, Array2D.length2 table

        if rows < 0 || columns < 0 then
            failwith $"MatrixData.tableToTree: Incorrect table data: %A{table}, rows %A{rows}, columns %A{columns}"

        elif rows = 0 || columns = 0 then
            QuadTree.None
        elif rows = 1 && columns = 1 then
            table[0, 0] |> getData
        // Matrix m by n.
        // Construct a matrix 2^n by 2^n from a given table.
        else
            let powerSize = Numbers.ceilPowTwo (max rows columns)

            let mtx = Matrix(table, 0, 0, powerSize, powerSize)

            // Make a SparseMatrix from the constructed matrix.
            maker mtx



open MatrixData

module SparseMatrix =

    type SparseMatrix<'Value when 'Value: equality>(arr2d: 'Value option[,]) =
        let data = tableToTree arr2d
        member this.Rows = Array2D.length1 arr2d
        member this.Columns = Array2D.length2 arr2d
        member this.Data = data



        member this.Item
            with get (i: int, j: int) =
                let rec search (i, j) size tree =

                    match tree with
                    | QuadTree.Leaf value -> Some value
                    | QuadTree.None -> Option.None
                    | QuadTree.Node(nw, ne, sw, se) ->

                        let middle = size / 2

                        let newI, newJ, quadrant =
                            if i <= middle && j <= middle then (i, j, nw)
                            elif i <= middle && j > middle then (i, j - middle, ne)
                            elif i > middle && j <= middle then (i - middle, j, sw)
                            else (i - middle, j - middle, se)

                        search (newI, newJ) middle quadrant

                let getValue (i, j) =

                    if i < 1 || j < 1 || i > this.Rows || j > this.Columns then
                        failwith $"SparseMatrix.Item with get(i, j): Indices %A{(i, j)} out of range."
                    else
                        let powerSize = Numbers.ceilPowTwo (max this.Rows this.Columns)
                        search (i, j) powerSize this.Data

                getValue (i, j)



open SparseMatrix

// module TreeAlgebra =

// let rec treeSum fAdd binTree1 binTree2 =
//     match binTree1, binTree2 with
//     // Neutral + Value = Value
//     | BinTree.None, tree -> tree
// | tree, BinTree.None -> tree
// | BinTree.Leaf _, BinTree.Node(b1, b2) ->
//     BinTree.Node(treeSum fAdd binTree1 b1, treeSum fAdd binTree1 b2)
// | BinTree.Node(a1, a2), BinTree.Leaf _ ->
//     BinTree.Node(treeSum fAdd a1 binTree2, treeSum fAdd a2 binTree2)
// | BinTree.Leaf a, BinTree.Leaf b -> BinTree.Leaf(fAdd a b)
// | BinTree.Node(a1, a2), BinTree.Node(b1, b2) ->
//     BinTree.Node(treeSum fAdd a1 b1, treeSum fAdd a2 b2)

(*
module MatrixAlgebra =

    /// Vector by Matrix multiplication.
    let vecByMtx fAdd fMult (vec: SparseVector<'a>) (mtx: SparseMatrix<'b>) : SparseVector<'c> =

        let rec inner fAdd fMult binTree quadTree =
            match binTree, quadTree with
            | BinTree.None, _
            | _, QuadTree.None -> BinTree.None
            | BinTree.Leaf a, QuadTree.Leaf b -> BinTree.Leaf(fMult a b)
            | BinTree.Leaf a, QuadTree.Node(b1, b2, b3, b4) ->
                let s1 = fAdd (inner fAdd fMult binTree b1) (inner fAdd fMult binTree b3)
                let s2 = fAdd (inner fAdd fMult binTree b2) (inner fAdd fMult binTree b4)
                BinTree.Node(s1, s2) |> VectorData.reduce
            | BinTree.Node(a1, a2), QuadTree.Leaf _ ->

                let s = fAdd (inner fAdd fMult a1 quadTree) (inner fAdd fMult a2 quadTree)
                BinTree.Node(s, s) |> VectorData.reduce

            | BinTree.Node(a1, a2), QuadTree.Node(b1, b2, b3, b4) ->


                let s1 = fAdd (inner fAdd fMult a1 b1) (inner fAdd fMult a2 b3)
                let s2 = fAdd (inner fAdd fMult a1 b2) (inner fAdd fMult a2 b4)
                BinTree.Node(s1, s2) |> VectorData.reduce


        if vec.Length <> mtx.Rows then

            failwith
                $"Algebra.vecByMtx: Dimensions of objects vector's length: %A{vec.Length} and Matrix rows %A{mtx.Rows} don't match."

        else
            inner fAdd fMult vec.Data mtx.Data
*)
