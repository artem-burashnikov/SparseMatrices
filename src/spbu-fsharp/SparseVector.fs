namespace SparseVector

open Trees.BinTrees
open System
open Helpers

module VectorData =

    type ArrVector<'A>(arr: array<Option<'A>>, head: uint, length: uint) =
        struct
            member this.Memory = arr
            member this.Head = head
            member this.Length = length
            member this.DataMaxIndex = arr.Length - 1 |> uint
        end


    type COOVector<'A>(list: List<uint * 'A>, length: uint) =
        member this.Data = list
        member this.Length = length


    /// Splits a given Vector in half.
    let arrVecPartition (vec: ArrVector<'A>) =
        let newLength = vec.Length / 2u
        ArrVector(vec.Memory, vec.Head, newLength), ArrVector(vec.Memory, vec.Head + newLength, newLength)


    /// Divide a given COOVector into 2 parts.
    /// Returns two vectors.
    let cooVecPartition (vec: COOVector<'A>) =
        // For an empty vector just return the data immediately.
        // Otherwise filter coordinates.
        if vec.Length = 0u then
            vec, vec
        else
            // Calculate middle point and filter coordinates by comparing
            // them to the middle point.
            let half = vec.Length / 2u

            let rec inner lst leftPart rightPart =
                match lst with
                | [] -> leftPart, rightPart
                | (i, value) :: tl ->
                    if i < half then
                        inner tl ((i, value) :: leftPart) rightPart
                    else
                        inner tl leftPart ((i - half, value) :: rightPart)

            // Construct and return the result.
            let leftPart, rightPart = inner vec.Data [] []

            COOVector(leftPart, half), COOVector(rightPart, half)


    /// Reduces identical data in a node of a binary tree to save space.
    let reduce binTreeNode =
        match binTreeNode with
        | BinTree.Node(BinTree.None, BinTree.None) -> BinTree.None
        | BinTree.Node(BinTree.Leaf a1, BinTree.Leaf a2) when a1 = a2 -> BinTree.Leaf a1
        | _ -> binTreeNode


    /// Stores a value (if anything) in a binary tree's branch.
    let store =
        function
        | Some value -> BinTree.Leaf value
        | Option.None -> BinTree.None


    /// Converts an array to Vector.
    let arrVecToTree (arr: array<Option<'A>>) =
        let rec maker (vec: ArrVector<'A>) =
            // If a given vector's starting index is outside of bounds of "memory",
            // then there is no need to store anything in a binary tree.
            if vec.Head > vec.DataMaxIndex then
                BinTree.None
            // If we find a 1x1 cell that is within the bounds of the vector's "memory".
            // Then look at data in that cell and store it accordingly.
            elif vec.Length = 1u then
                let index = Convert.ToInt32 vec.Head
                vec.Memory[index] |> store
            // Otherwise split the vector in half
            // and recursively call the maker function on the resulting halves.
            else
                let leftPart, rightPart = arrVecPartition vec

                BinTree.Node(maker leftPart, maker rightPart) |> reduce

        let vec = ArrVector(arr, 0u, arr.Length |> uint)

        // Special cases to save space in a resulting binary tree.
        // i.e. BinTree.Leaf('a) instead of BinTree.Node(BinTree.Leaf('a), BinTree.None)
        if vec.Length = 0u then
            BinTree.None
        elif (vec.Length = 1u) then
            let index = Convert.ToInt32 vec.Head
            vec.Memory[index] |> store
        else
            // In general case we upsize a given vector's length to a power of 2 so that the data correctly gets split in half on each recursive call.
            // Result is a healthy binary tree.
            let powerSize = Numbers.ceilPowTwo (arr.Length |> uint)

            let vec = ArrVector(arr, 0u, powerSize)
            maker vec


    let cooVecToTree (vec: COOVector<'A>) =

        let maxDataIndex = vec.Length - 1u

        let rec maker (vec: COOVector<'A>) =

            if vec.Length = 1u && vec.Data.Length = 1 && (fst vec.Data.Head) <= maxDataIndex then
                BinTree.Leaf(snd vec.Data.Head)
            elif vec.Data.Length < 1 then
                BinTree.None
            else
                let leftPart, rightPart = cooVecPartition vec

                BinTree.Node(maker leftPart, maker rightPart) |> reduce

        if vec.Length = 0u then
            BinTree.None
        elif vec.Length = 1u && vec.Data.Length <> 0 then
            BinTree.Leaf(snd vec.Data.Head)
        else
            let powerSize = Numbers.ceilPowTwo vec.Length
            COOVector(vec.Data, powerSize) |> maker


open VectorData

module SparseVector =

    type SparseVector<'A when 'A: equality> =
        val Data: BinTree<'A>
        val Length: uint

        new(tree, length) = { Data = tree; Length = length }

        new(arr) =
            { Data = arrVecToTree arr
              Length = arr.Length |> uint }

        new(cooVec) =
            { Data = cooVecToTree cooVec
              Length = cooVec.Length }

        new(cooList, length) =
            { Data = COOVector(cooList, length) |> cooVecToTree
              Length = length }

        member this.Item
            with get i =

                // Given an index i (starts at 1) we want to find a corresponding data in a tree.
                // By recalling how the tree was constructed we recursively call each node's child according to
                // which part the i would fall into. Basically a binary search algorithm.
                //
                //     i      middle
                // ------------||------------
                //
                let rec search i size tree =
                    match tree with
                    | BinTree.Leaf value -> Some value
                    | BinTree.None -> Option.None
                    | BinTree.Node(leftChild, rightChild) ->
                        let middle = size / 2u

                        if i < middle then
                            search i middle leftChild
                        else
                            search (i - middle) middle rightChild

                let getValue i =
                    if i >= this.Length then
                        failwith $"SparseVector.Item with get(i): Index %A{i} is out of range."
                    else
                        let powerSize = Numbers.ceilPowTwo this.Length
                        search i powerSize this.Data

                getValue i

        member this.IsEmpty = this.Data = BinTree.None
