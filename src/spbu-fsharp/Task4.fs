module HomeWork4

open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees

/// This sub-structure is used to build a Binary Tree from a given array.
// The constructed Binary Tree will be then used as a data storage for a sparse vector.
[<Struct>]
type Vector<'value> =
    // Initial vector data.
    val Data: array<'value>
    // Initialize two pointer.
    val Left: int
    val Right: int

    new(data, left, right) =
        { Data = data
          Left = left
          Right = right }

    member this.CurrentLength = this.Right - this.Left + 1

    /// The maximum index that this array would have if its length were equal to a power of two.
    member this.paddedIndex = (Numbers.ceilPowTwo this.Data.Length) - 1

    /// Maximum real available index.
    member this.DataMaxIndex = this.Data.Length - 1

/// Splits a given Vector in half.
let vecPartition (vec: Vector<'value option>) =
    let newRight = (vec.Left + vec.Right) / 2
    Vector(vec.Data, vec.Left, newRight), Vector(vec.Data, newRight + 1, vec.Right)


let vecToTree (vec: Vector<'value option>) =

    // Pad a given vector with indices.
    let paddedVec = Vector(vec.Data, vec.Left, vec.paddedIndex)


    let rec maker (paddedVec: Vector<'value option>) =
        // If we look at a vector which starting index is out of bounds,
        // then there is no real data in it, no need to store anything.
        if paddedVec.Left > paddedVec.DataMaxIndex then
            BinTree.None
        // If we find a cell that is within the bounds of the initial array,
        // then we look at the data in that cell and store it accordingly.
        elif (paddedVec.CurrentLength = 1)
             && (paddedVec.Left <= paddedVec.DataMaxIndex) then
            let data = paddedVec.Data[paddedVec.Left]

            match data with
            | Some value -> Leaf value
            | Option.None -> BinTree.None
        else
            // Otherwise we split the array (padded with indices to the power of 2) in two halves
            // and repeat the process.
            let leftPart, rightPart = vecPartition paddedVec

            // To get rid of the unnecessary data we merge a node into a single one
            // if the node has no value in its children.
            let result = Node(maker leftPart, maker rightPart)

            match result with
            | Node (BinTree.None, BinTree.None) -> BinTree.None
            | _ -> result

    maker paddedVec
