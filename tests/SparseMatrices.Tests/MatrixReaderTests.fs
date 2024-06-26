namespace SparseMatrices.MatrixReader.Tests

open Expecto
open Microsoft.FSharp.Core
open SparseMatrices.MatrixReader
open SparseMatrices.Helpers
open SparseMatrices.SparseMatrix

module FileReading =

    let src = __SOURCE_DIRECTORY__
    let folder = "external-test-data"

    [<Tests>]
    let tests =

        testList
            "File reading cases"
            [ testCase "Empty file"
              <| fun _ ->
                  let fp = System.IO.Path.Combine [| src; folder; "empty-file" |]
                  Expect.throws (fun _ -> MatrixReader(fp) |> ignore) "Empty file or not existing file was given"

              testCase "File is not empty, but is not a matrix market file"
              <| fun _ ->
                  let fp = System.IO.Path.Combine [| src; folder; "incorrect-file-type" |]
                  Expect.throws (fun _ -> MatrixReader(fp) |> ignore) "Not a matrix market file"

              testCase "General float value matrix"
              <| fun _ ->
                  let fp = System.IO.Path.Combine [| src; folder; "general-real.mtx" |]
                  let actualResult = MatrixReader(fp).Read(real)

                  let expectedResult =
                      let data =
                          [ (0u, 0u, 2832268.51852)
                            (4u, 0u, parseFloat "1e6")
                            (5u, 0u, 2083333.33333)
                            (6u, 0u, -3333.33333333)
                            (7u, 0u, parseFloat "2e6") ]

                      COOMatrix(data, 8u, 8u) |> SparseMatrix

                  Expect.equal actualResult.Data expectedResult.Data "Matrices don't match"

              testCase "General pattern value matrix"
              <| fun _ ->
                  let fp = System.IO.Path.Combine [| src; folder; "general-real.mtx" |]
                  let actualResult = MatrixReader(fp).Read(pattern)

                  let expectedResult =
                      let data = [ (0u, 0u, ()); (4u, 0u, ()); (5u, 0u, ()); (6u, 0u, ()); (7u, 0u, ()) ]
                      COOMatrix(data, 8u, 8u) |> SparseMatrix

                  Expect.equal actualResult.Data expectedResult.Data "Matrices don't match"

              testCase "General integer value matrix"
              <| fun _ ->
                  let fp = System.IO.Path.Combine [| src; folder; "general-integer.mtx" |]
                  let actualResult = MatrixReader(fp).Read(integer)

                  let expectedResult =
                      let data = [ (0u, 0u, 2); (4u, 0u, 1); (5u, 0u, 20); (6u, 0u, -3); (7u, 0u, 1) ]
                      COOMatrix(data, 8u, 8u) |> SparseMatrix

                  Expect.equal actualResult.Data expectedResult.Data "Matrices don't match" ]
