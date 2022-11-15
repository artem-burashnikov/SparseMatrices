namespace spbu_fsharp

open HomeWork4
open Microsoft.FSharp.Core

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let arr1 = [| Some 1; Some 1; Some 1 |]
        let arr2 = [| Some 2; Some 2; Some 3 |]
        let vec1 = SparseVector.SparseVector arr1
        printfn $"%A{vec1.Data}"
        let vec2 = SparseVector.SparseVector arr2
        printfn $"%A{vec2.Data}"
        let result = MatrixAlgebra.vecPlusVec (0) (0) (0) (+) vec1 vec2
        printfn $"%A{result.Data}"
        0
