module HomeWork4
open Helpers
open Microsoft.FSharp.Core
open Trees.BinTrees

[<Struct>]
type Vector<'value> =
    val Data: array<'value>
    val Left: int
    val Right: int
    new(data, left, right) = {Data = data; Left = left; Right = right}
    member this.Length = this.Right - this.Left + 1
    member this.paddedIndex = (Numbers.ceilPowTwo this.Data.Length) - 1


let vecPartition (vec: Vector<'value>) =
    let newRight = (vec.Left + vec.Right) / 2
    Vector(vec.Data, vec.Left, newRight),
    Vector(vec.Data, newRight + 1, vec.Right)


let rec vecToTree (vec: Vector<'value option>) =

    let paddedVec = Vector(vec.Data, 0, vec.paddedIndex)

    let rec maker (paddedVec: Vector<'value option>) =
            if paddedVec.Left > paddedVec.Data.Length - 1 then
                BinTree.None
            elif (paddedVec.Length = 1) && (paddedVec.Left <= (paddedVec.Data.Length - 1)) then
                let data = paddedVec.Data[paddedVec.Left]
                match data with
                | Some value -> Leaf value
                | Option.None -> BinTree.None
            else
                let leftPart, rightPart = vecPartition paddedVec
                // printfn $"%A{leftPart.Data[leftPart.Left..leftPart.Right]}"
                // printfn $"%A{rightPart.Data[rightPart.Left..rightPart.Right]}"
                // System.Console.ReadKey() |> ignore
                let result = Node(maker leftPart, maker rightPart)
                match result with
                | Node(BinTree.None, BinTree.None) -> BinTree.None
                | _ -> result

    maker paddedVec
