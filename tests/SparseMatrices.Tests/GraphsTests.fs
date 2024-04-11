module SparseMatrices.Graphs.Tests

open SparseMatrices.SparseMatrix
open SparseMatrices.Graphs
open SparseMatrices.Helpers
open Expecto
open Microsoft.FSharp.Core

module TestCases =

    let naiveEdgesOfMtx (table: int option[,]) =
        seq {
            for i = 0 to Array2D.length1 table - 1 do
                for j = 0 to Array2D.length2 table - 1 do
                    let value = table[i, j]

                    if value.IsSome then
                        yield uint i, uint j
        }
        |> Set.ofSeq

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
                  let actualResult = graph.Edges
                  let expectedResult = naiveEdgesOfMtx table

                  Expect.equal
                      (Set.isSubset actualResult expectedResult)
                      (Set.isSubset expectedResult actualResult)
                      $"The results were different actual result: %A{actualResult}\n expected result: %A{expectedResult}\n %A{table}"

              testProperty "Getting Edges from 'collapsed' SparseMatrix"
              <| fun (rows: uint) (columns: uint) (value: int option) ->
                  let table = Array2D.create (toIntConv (rows + 2u)) (toIntConv (columns + 2u)) value
                  let graph = SparseMatrix table |> Graph
                  let actualResult = graph.Edges
                  let expectedResult = naiveEdgesOfMtx table

                  Expect.equal
                      (Set.isSubset actualResult expectedResult)
                      (Set.isSubset expectedResult actualResult)
                      $"The results were different actual result: %A{actualResult}\n expected result: %A{expectedResult}\n %A{table}" ]
