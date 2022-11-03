module HomeWork4

open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees



/// This sub-structure is used to build a Binary Tree from a given array.
// The constructed Binary Tree will be then used as a data storage for a sparse vector.
[<Struct>]
type Vector<'value> =
    /// Initial vector data.
    val Data: array<'value>
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



/// Splits a given Vector in half.
let vecPartition (vec: Vector<'value>) =
    let newRight = (vec.Left + vec.Right) / 2
    Vector(vec.Data, vec.Left, newRight), Vector(vec.Data, newRight + 1, vec.Right)



let vecToTree (arr: array<'value option>) =
    let rec maker (vec: Vector<'value option>) =
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

            // To get rid of the unnecessary data or to store adjacent identical data more efficiently
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
