module GraphsTests

open SparseMatrix.SparseMatrix
open Graphs
open Helpers.Numbers
open Expecto
open Microsoft.FSharp.Core

module TestCases =


    let init2DArrayWithDensity (density: int) rows columns =
        let table = Array2D.create rows columns Option.None

        for i in 0 .. rows - 1 do
            for j in 0 .. columns - 1 do
                let cellDensity = (float (i * columns + j + 1) / float (rows * columns)) * 100.0

                if cellDensity <= density then
                    table[i, j] <- Some(i + j + 1)

        table

    let naiveEdgesOfMtx (table: int option[,]) : Set<UndirectedEdge<uint>> =
        Set.ofSeq (
            seq {
                for i = 0 to Array2D.length1 table - 1 do
                    for j = 0 to Array2D.length2 table - 1 do
                        let value = table[i, j]

                        if value <> Option.None then
                            yield (Set.ofList [ uint i; uint j ])
            }
        )

    [<Tests>]
    let tests =
        testList
            "samples"
            [

              testProperty "Edges from Matrix: naive approach should produce the same result"
              <| fun (rows: uint) (columns: uint) ->
                  let table =
                      init2DArrayWithDensity (toIntConv (rows + 2u)) (toIntConv (columns + 2u)) 50

                  let graph = SparseMatrix table |> Graph
                  let set1 = graph.Edges
                  let set2 = naiveEdgesOfMtx table

                  Expect.equal
                      (Set.isSubset set1 set2)
                      (Set.isSubset set2 set1)
                      $"The results were different set1: %A{set1}\n set2: %A{set2}\n %A{table}"

              ]
