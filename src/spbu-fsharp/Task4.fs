namespace HomeWork4

open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees
open Trees.QuadTrees


module VectorData =

    /// This sub-structure is used to build a Binary Tree from a given array.
    // The constructed Binary Tree will be then used as a data storage for a sparse vector.
    type Vector<'Value> =
        struct
            /// Initial vector data.
            val Data: array<'Value option>
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
    let vecPartition (vec: Vector<'Value>) =
        let newRight = (vec.Left + vec.Right) / 2
        Vector(vec.Data, vec.Left, newRight), Vector(vec.Data, newRight + 1, vec.Right)


    let getData =
        function
        | Some value -> BinTree.Leaf value
        | Option.None -> BinTree.None


    let vecToTree (arr: array<'Value option>) =
        let rec maker (vec: Vector<'Value>) =
            // If we find a cell that is within the bounds of the initial array,
            // then we look at the data in that cell and store it accordingly.
            // Special case for an array of a single element is needed,
            // since the resulting tree would contain unnecessary branch otherwise.
            if vec.Data.Length = 1 || ((vec.Length = 1) && (vec.Left <= vec.DataMaxIndex)) then
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
                | BinTree.Node(BinTree.None, BinTree.None) -> BinTree.None
                | BinTree.Node(BinTree.Leaf value1, BinTree.Leaf value2) when value1 = value2 -> BinTree.Leaf value1
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

    type SparseVector<'Value> =
        val Length: int
        val Data: BinTree<'Value>

        new(length, data) = { Data = data; Length = length }

        member this.Item
            with get i =

                /// Navigates search function through a binary tree.
                let look (direction: Direction) (tree: BinTree<'Value>) =
                    match direction, tree with
                    | Left, BinTree.Node(leftChild, _) -> leftChild
                    | Right, BinTree.Node(_, rightChild) -> rightChild
                    | _, BinTree.Leaf _ -> tree
                    | _, BinTree.None -> tree


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
                    let ix = i - 1 // Vector's index (i) starts at 1, so we offset by one.

                    if ix < 0 || ix > this.Length - 1 then
                        failwith $"SparseVector.Item with get(i): Index %A{ix} is out of range."
                    else
                        search ix 0 (Numbers.ceilPowTwo this.Length - 1) this.Data

                getValue i


    // Construct a SparseVector from a given array.
    let toSparse (arr: 'Value option array) =
        let length = arr.Length
        let data = VectorData.vecToTree arr
        SparseVector(length, data)



open SparseVector

module MatrixData =

    type Matrix<'Value> =
        struct
            // Initial matrix data.
            val Data: array<array<'Value option>>
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


    let mtxPartition (mtx: Matrix<'Value>) =
        if (mtx.Data.Length = 0) || (mtx.Data.Length = 1 && mtx.Data[0].Length = 0) then
            Matrix([||], 0, 0, -1, -1, -1, -1),
            Matrix([||], 0, 0, -1, -1, -1, -1),
            Matrix([||], 0, 0, -1, -1, -1, -1),
            Matrix([||], 0, 0, -1, -1, -1, -1)
        else
            let newX = (mtx.Ix + mtx.Jx) / 2
            let newY = (mtx.Iy + mtx.Jy) / 2
            //
            //   |Ixy       |           |
            //   |    NW    |    NE     |
            //  _|__________|___________|_
            //   |          |           |
            //   |    SW    |    SE  Jxy|
            //
            Matrix(mtx.Data, mtx.DataRows, mtx.DataCols, mtx.Ix, mtx.Iy, newX, newY), // NW
            Matrix(mtx.Data, mtx.DataRows, mtx.DataCols, mtx.Ix, newY + 1, newX, mtx.Jy), // NE
            Matrix(mtx.Data, mtx.DataRows, mtx.DataCols, newX + 1, mtx.Iy, mtx.Jx, newY), // SW
            Matrix(mtx.Data, mtx.DataRows, mtx.DataCols, newX + 1, newY + 1, mtx.Jx, mtx.Jy) // SE


    let getData =
        function
        | Some value -> QuadTree.Leaf value
        | Option.None -> QuadTree.None


    let allEqual a b c d = a = b && a = c && a = d


    /// This function coverts a given table into a QudTree by splitting data into 4 quadrants.
    let mtxToTree (table: array<array<'Value option>>) =
        let rec maker (mtx: Matrix<'Value>) =

            // If we find a cell within bounds of the original data, then store the cell's value accordingly.
            if
                mtx.LengthX = 1
                && mtx.LengthY = 1
                && mtx.Jx <= (mtx.DataRows - 1)
                && mtx.Jy <= (mtx.DataCols - 1)
            then

                mtx.Data[mtx.Ix][mtx.Iy] |> getData

            // If the Quadrant is fully out of range of the original data, no need to look further.
            // Otherwise we partition the Quadrant again.
            elif mtx.Iy > (mtx.DataCols - 1) || mtx.Ix > (mtx.DataRows - 1) then
                QuadTree.None
            else

                let nw, ne, sw, se = mtxPartition mtx

                let node = QuadTree.Node(maker nw, maker ne, maker sw, maker se)

                match node with
                | QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.None) -> QuadTree.None
                | QuadTree.Node(QuadTree.Leaf nw, QuadTree.Leaf ne, QuadTree.Leaf sw, QuadTree.Leaf se) when
                    allEqual nw ne sw se
                    ->
                    QuadTree.Leaf nw
                | _ -> node

        // Empty Matrix cases.
        if (table.Length = 0) || (table.Length = 1 && table[0].Length = 0) then
            QuadTree.None
        // Matrix 1x1
        elif table.Length = 1 && table[0].Length = 1 then
            table[0][0] |> getData
        // Matrix m by n.
        // Construct a matrix 2^n by 2^n (n > 1) from a given table.
        elif table.Length > 0 && table[0].Length > 0 then
            let paddedIndex = Numbers.ceilPowTwo (max table.Length table[0].Length) - 1


            let mtx =
                Matrix(table, table.Length, table[0].Length, 0, 0, paddedIndex, paddedIndex)

            // Make a SparseMatrix from the constructed matrix.
            maker mtx
        else
            failwith $"MatrixData.mtxToTree: Incorrect table data: %A{table}"



module SparseMatrix =

    // 4 quadrants in a QuadTree's node.
    type Direction =
        | NW
        | NE
        | SW
        | SE


    type SparseMatrix<'Value> =
        val Rows: int
        val Columns: int
        val Data: QuadTree<'Value>

        new(rows, columns, data) =
            { Rows = rows
              Columns = columns
              Data = data }

        member this.Item
            with get (i, j) =

                /// Navigates search function through a QuadTree.
                let look (direction: Direction) (tree: QuadTree<'Value>) =
                    match direction, tree with
                    | NW, QuadTree.Node(nw, _, _, _) -> nw
                    | NE, QuadTree.Node(_, ne, _, _) -> ne
                    | SW, QuadTree.Node(_, _, sw, _) -> sw
                    | SE, QuadTree.Node(_, _, _, se) -> se
                    | _, QuadTree.Leaf _ -> tree
                    | _, QuadTree.None -> tree

                // i, j - indices we are looking for.
                // ix, iy - coordinates of the top-left cell in a quadrant.
                // jx, jy - coordinates of the bottom-right cell in a quadrant.
                let rec search i j ix iy jx jy tree =

                    let middleI = (ix + jx) / 2
                    let middleJ = (iy + jy) / 2

                    // Data is located in the NW quadrant.
                    if i <= middleI && j <= middleJ then
                        let node = look NW tree

                        match node with
                        | QuadTree.Leaf value -> Some value
                        | QuadTree.None -> Option.None
                        | _ -> search i j ix iy middleI middleJ node

                    // Data is located in the NE quadrant.
                    elif i <= middleI && j > middleJ then
                        let node = look NE tree

                        match node with
                        | QuadTree.Leaf value -> Some value
                        | QuadTree.None -> Option.None
                        | _ -> search i j ix (middleJ + 1) middleI jy node

                    // Data is located in the SW quadrant.
                    elif i > middleI && j <= middleJ then
                        let node = look SW tree

                        match node with
                        | QuadTree.Leaf value -> Some value
                        | QuadTree.None -> Option.None
                        | _ -> search i j (middleI + 1) iy jx middleJ node

                    // Data is located in the SE quadrant.
                    else
                        let node = look SE tree

                        match node with
                        | QuadTree.Leaf value -> Some value
                        | QuadTree.None -> Option.None
                        | _ -> search i j (middleI + 1) (middleJ + 1) jx jy node


                let getValue (i: int) (j: int) =
                    // Matrix indices (i, j) start at 1, so we offset by one.
                    let rowIndex = i - 1
                    let colIndex = j - 1

                    if
                        rowIndex < 0
                        || colIndex < 0
                        || rowIndex > this.Rows - 1
                        || colIndex > this.Columns - 1
                    then
                        failwith $"SparseMatrix.Item with get(i, j): Indices %A{(i, j)} out of range."
                    else
                        let paddedIndex = Numbers.ceilPowTwo (max this.Rows this.Columns) - 1
                        search rowIndex colIndex 0 0 paddedIndex paddedIndex this.Data

                getValue i j


    /// Convert a table to SparseMatrix.
    let toSparse (table: array<array<'Value option>>) =
        let rows, columns =
            if table.Length = 0 then 0, 0
            elif table.Length = 1 && table[0].Length = 0 then 0, 0
            else table.Length, table[0].Length

        let data = MatrixData.mtxToTree table
        SparseMatrix(rows, columns, data)



module TreeAlgebra =

    /// Sum of two binary trees.
    /// This does not cover the case when trees have different depth
    /// since it's assumed the incorrect data will be filtered in the higher function call.
    let rec treeSum fAdd binTree1 binTree2 =
        let recurse = treeSum fAdd

        match binTree1, binTree2 with
        // Neutral + Value = Value
        | BinTree.None, tree
        | tree, BinTree.None -> tree
        // This, in fact, is the following case: BinTree.Node(a, a), BinTree.Node(b1, b2) and vice versa.
        // Since it's assumed that trees have matching depths, summation continues as if the node had two children.
        | BinTree.Leaf _, BinTree.Node(b1, b2) -> BinTree.Node(recurse binTree1 b1, recurse binTree1 b2)
        | BinTree.Node(a1, a2), BinTree.Leaf _ -> BinTree.Node(recurse a1 binTree2, recurse a2 binTree2)
        | BinTree.Leaf a, BinTree.Leaf b -> BinTree.Leaf(fAdd a b)
        | BinTree.Node(a1, a2), BinTree.Node(b1, b2) -> BinTree.Node(recurse a1 b1, recurse a2 b2)


    /// Algebraic operation on elements.
    let fDo operator a b = operator a b


    /// Vector by Matrix multiplication.
    let vecByMtx fAdd fMult (vec: SparseVector.SparseVector<'Value>) (mtx: SparseMatrix.SparseMatrix<'Value>) =

        if vec.Length <> mtx.Rows then

            failwith
                $"Algebra.vecByMtx: Dimensions of objects vector's length: %A{vec.Length} and Matrix rows %A{mtx.Rows} don't match."

        else

            let rec inner fAdd fMult binTree quadTree =
                let recurse = inner fAdd fMult

                match binTree, quadTree with
                // Neutral * Value = Neutral.
                | BinTree.None, _
                | _, QuadTree.None -> BinTree.None

                | BinTree.Leaf a, QuadTree.Leaf b -> BinTree.Leaf(fMult a b)

                // Left argument is in fact BinTree.Node(Leaf a, Leaf a)
                | BinTree.Leaf _, QuadTree.Node(b1, b2, b3, b4) ->


                    let s1 = fAdd (recurse binTree b1) (recurse binTree b3)
                    let s2 = fAdd (recurse binTree b2) (recurse binTree b4)
                    BinTree.Node(s1, s2)

                // Right argument is in fact Quadtree.Node(Leaf a, Leaf a, Leaf a, Leaf a)
                | BinTree.Node(a1, a2), QuadTree.Leaf _ ->

                    let s = fAdd (recurse a1 quadTree) (recurse a2 quadTree)
                    BinTree.Node(s, s)

                | BinTree.Node(a1, a2), QuadTree.Node(b1, b2, b3, b4) ->


                    let s1 = fAdd (recurse a1 b1) (recurse a2 b3)
                    let s2 = fAdd (recurse a1 b2) (recurse a2 b4)
                    BinTree.Node(s1, s2)

            let data = inner fAdd fMult vec.Data mtx.Data
            SparseVector(mtx.Columns, data)
