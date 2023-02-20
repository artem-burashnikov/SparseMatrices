module GraphsTests

open SparseMatrix.SparseMatrix
open Graphs
open Helpers.Numbers
open Helpers.Initializers
open Expecto
open Microsoft.FSharp.Core

module TestCases =

    let naiveEdgesOfMtx (table: int option[,]) =
        Set.ofSeq (
            seq {
                for i = 0 to Array2D.length1 table - 1 do
                    for j = 0 to Array2D.length2 table - 1 do
                        let value = table[i, j]

                        if value.IsSome then
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
