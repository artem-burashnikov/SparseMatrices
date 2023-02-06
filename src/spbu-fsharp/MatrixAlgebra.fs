namespace MatrixAlgebra

open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees
open Trees.QuadTrees
open SparseVector.SparseVector
open SparseMatrix.SparseMatrix

module MatrixAlgebra =

    let getValue x =
        match x with
        | Some a -> a
        | _ -> failwith "Only accepts Some value"


    let convertResult value =
        match value with
        | Option.None -> BinTree.None
        | _ -> BinTree.Leaf(value |> getValue)

    /// Adds two Sparse vectors together utilizing parallel computations.
    let vectorMap2 computationLevel mapping (vec1: SparseVector<'A>) (vec2: SparseVector<'B>) : SparseVector<'C> =

        let asyncCompute func parallelLevel a1 a2 b1 b2 =
            async {
                let tasks =
                    [| async { return func (parallelLevel - 1) a1 b1 }
                       async { return func (parallelLevel - 1) a2 b2 } |]

                let! results = tasks |> Async.Parallel
                return BinTree.Node(results[0], results[1]) |> SparseVector.VectorData.reduce
            }

        let rec inner parallelLevel bTree1 bTree2 =
            match bTree1, bTree2 with
            | BinTree.None, BinTree.None -> mapping Option.None Option.None |> convertResult

            | BinTree.None, BinTree.Leaf b -> mapping Option.None (Some b) |> convertResult

            | BinTree.Leaf a, BinTree.None -> mapping (Some a) Option.None |> convertResult

            | BinTree.Leaf a, BinTree.Leaf b -> mapping (Some a) (Some b) |> convertResult

            | BinTree.None, BinTree.Node(b1, b2) ->
                if parallelLevel = 0 then
                    BinTree.Node(inner 0 bTree1 b1, inner 0 bTree1 b2)
                    |> SparseVector.VectorData.reduce
                else
                    asyncCompute inner parallelLevel bTree1 bTree1 b1 b2 |> Async.RunSynchronously

            | BinTree.Node(a1, a2), BinTree.None ->
                if parallelLevel = 0 then
                    BinTree.Node(inner 0 a1 bTree2, inner 0 a2 bTree2)
                    |> SparseVector.VectorData.reduce
                else
                    asyncCompute inner parallelLevel a1 a2 bTree2 bTree2 |> Async.RunSynchronously

            | BinTree.Leaf _, BinTree.Node(b1, b2) ->
                if parallelLevel = 0 then
                    BinTree.Node(inner 0 bTree1 b1, inner 0 bTree1 b2)
                    |> SparseVector.VectorData.reduce
                else
                    asyncCompute inner parallelLevel bTree1 bTree1 b1 b2 |> Async.RunSynchronously

            | BinTree.Node(a1, a2), BinTree.Leaf _ ->
                if parallelLevel = 0 then
                    BinTree.Node(inner 0 a1 bTree2, inner 0 a2 bTree2)
                    |> SparseVector.VectorData.reduce
                else
                    asyncCompute inner parallelLevel a1 a2 bTree2 bTree2 |> Async.RunSynchronously

            | BinTree.Node(a1, a2), BinTree.Node(b1, b2) ->
                if parallelLevel = 0 then
                    BinTree.Node(inner 0 a1 b1, inner 0 a2 b2) |> SparseVector.VectorData.reduce
                else
                    asyncCompute inner parallelLevel a1 a2 b1 b2 |> Async.RunSynchronously

        if vec1.Length <> vec2.Length then
            failwith "parallelElementwiseVecVec: Dimensions of objects don't match."
        else
            SparseVector(inner computationLevel vec1.Data vec2.Data, vec1.Length)


    /// Vector by Matrix multiplication utilizing parallel computations.
    let vecByMtx
        computationLevel
        fAdd
        (fMult: Option<'A> -> Option<'B> -> Option<'C>)
        (vec: SparseVector<'A>)
        (mtx: SparseMatrix<'B>)
        : SparseVector<'C> =

        // We need to calculate how deep is the deepest tree.
        // We do it by recalling how it was constructed.
        // Now and later we may only use number of Rows of a given matrix for necessary calculations,
        // since a given vector's length would have to match it to produce the result.
        let powerSize = (max mtx.Rows mtx.Columns) |> Numbers.ceilPowTwo
        let maxDepth = Numbers.powTwo powerSize

        // Multiplication optimization.
        // We need to calculate n sums and the first sum is already given, so the counter stops at 1 (starts at n).
        let rec multMult value counter =
            if counter = 1u then
                value
            else
                multMult (fAdd value value) (counter - 1u)

        /// Parallel multiplication workflow.
        let rec inner parallelLevel bTree qTree depth : BinTree<'C> =

            let fDo parallelLevel bTree1 bTree2 qTree1 qTree2 depth =
                if parallelLevel = 0 then
                    let vec1 = SparseVector((inner 0 bTree1 qTree1 (depth + 1u)), mtx.Columns)
                    let vec2 = SparseVector(inner 0 bTree2 qTree2 (depth + 1u), mtx.Columns)
                    let result = vectorMap2 0 fAdd vec1 vec2
                    result.Data
                else
                    let tasks =
                        [| async { return SparseVector((inner parallelLevel bTree1 qTree1 (depth + 1u)), mtx.Columns) }
                           async { return SparseVector(inner parallelLevel bTree2 qTree2 (depth + 1u), mtx.Columns) } |]

                    let vectors = tasks |> Async.Parallel |> Async.RunSynchronously
                    let result = vectorMap2 parallelLevel fAdd vectors[0] vectors[1]
                    result.Data

            let asyncCompute func parallelLevel a1 a2 nw ne sw se depth =
                let tasks =
                    [| async { return func (parallelLevel - 1) a1 a2 nw sw depth }
                       async { return func (parallelLevel - 1) a1 a2 ne se depth } |]

                let results = tasks |> Async.Parallel |> Async.RunSynchronously
                BinTree.Node(results[0], results[1]) |> SparseVector.VectorData.reduce


            match bTree, qTree with
            // Neutral * Neutral
            | BinTree.None, _
            | _, QuadTree.None -> BinTree.None

            // Value * Value.
            // If both Leaves are met at the maximum depth, then just multiply values.
            // Otherwise the result is calculated over n summations,
            // where n is the difference between the maximum depth and the current tree level (starts at 0).
            | BinTree.Leaf a, QuadTree.Leaf b ->
                if depth = maxDepth then
                    fMult (Some a) (Some b) |> convertResult

                else
                    let result = fAdd (fMult (Some a) (Some b)) (fMult (Some a) (Some b))

                    match result with
                    | Option.None -> BinTree.None
                    | _ -> BinTree.Leaf(multMult result (maxDepth - depth) |> getValue)

            // BinTree.Leaf _ is actually BinTree.Node(a, a)
            | BinTree.Leaf _, QuadTree.Node(nw, ne, sw, se) ->
                if parallelLevel = 0 then
                    BinTree.Node(fDo 0 bTree bTree nw sw depth, fDo 0 bTree bTree ne se depth)
                    |> SparseVector.VectorData.reduce
                else
                    asyncCompute fDo parallelLevel bTree bTree nw ne sw se depth

            // QuadTree.Leaf _ is actually QuadTree.Node(b, b, b, b)
            | BinTree.Node(a1, a2), QuadTree.Leaf _ ->
                let result = fDo parallelLevel a1 a2 qTree qTree depth
                BinTree.Node(result, result) |> SparseVector.VectorData.reduce

            | BinTree.Node(a1, a2), QuadTree.Node(nw, ne, sw, se) ->
                if parallelLevel = 0 then
                    BinTree.Node(fDo 0 a1 a2 nw sw depth, fDo 0 a1 a2 ne se depth)
                    |> SparseVector.VectorData.reduce
                else
                    asyncCompute fDo parallelLevel a1 a2 nw ne sw se depth

        /// Chop unnecessary branches of the result of multiplication.
        let rec chopChop treeLevel size bTree =
            if treeLevel < size then
                match bTree with
                | BinTree.Node(left, _) -> chopChop (treeLevel + 1u) size left
                | BinTree.Leaf _
                | BinTree.None -> bTree
            else
                bTree

        // Pad the much needed branches so the multiplication actually works as intended.
        let rec powerUp treeLevel size bTree =
            if treeLevel < size then
                match bTree with
                | BinTree.Node _ -> BinTree.Node(powerUp (treeLevel + 1u) size bTree, BinTree.None)
                | BinTree.Leaf _
                | BinTree.None -> bTree
            else
                bTree

        // Calculate how many tree levels would we need to pad or to chop.
        // We then would start at 0 ang go up to that number.
        let mtxRowPower = mtx.Rows |> Numbers.powTwo
        let mtxColumnPower = mtx.Columns |> Numbers.powTwo
        let diff = (max mtxColumnPower mtxRowPower) - (min mtxRowPower mtxColumnPower)


        if vec.Length <> mtx.Rows then

            failwith
                $"Algebra.vecByMtx: Dimensions of objects vector's length: %A{vec.Length} and Matrix rows %A{mtx.Rows} don't match."

        // 1x1 is an edge case.
        elif mtx.Rows = 1u && mtx.Columns = 1u then
            let tree = fMult vec[0u] mtx[0u, 0u] |> convertResult

            SparseVector(tree, mtx.Columns)

        // In this case we would have to pad the vector's length before multiplication.
        elif mtxRowPower < mtxColumnPower then
            let tree = inner computationLevel (powerUp 0u diff vec.Data) mtx.Data 0u
            SparseVector(tree, mtx.Columns)

        // When sizes match no additional action is needed.
        elif mtxRowPower = mtxColumnPower then
            let tree = inner computationLevel vec.Data mtx.Data 0u
            SparseVector(tree, mtx.Columns)
        else
            // Otherwise we need to clean up the result by chopping.
            let tree = inner computationLevel vec.Data mtx.Data 0u |> chopChop 0u diff
            SparseVector(tree, mtx.Columns)
