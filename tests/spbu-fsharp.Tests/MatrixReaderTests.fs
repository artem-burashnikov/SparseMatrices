namespace MatrixReaderTests

open Expecto
open Microsoft.FSharp.Core
open MatrixReader
open Helpers.Numbers
open SparseMatrix.MatrixData
open SparseMatrix.SparseMatrix


module FileReading =

    [<Tests>]
    let tests =

        testList
            "File reading cases"
            [ testCase "Empty file"
              <| fun _ ->
                  let fp = __SOURCE_DIRECTORY__ + "external-test-data/empty-file"
                  Expect.throws (fun _ -> MatrixReader(fp) |> ignore) "Empty file or not existing file was given"

              testCase "File is not empty, but is not a matrix market file"
              <| fun _ ->
                  let fp = __SOURCE_DIRECTORY__ + "/external-test-data/incorrect-file-type"
                  Expect.throws (fun _ -> MatrixReader(fp) |> ignore) "Not a matrix market file"

              testCase "General float value matrix"
              <| fun _ ->
                  let fp = __SOURCE_DIRECTORY__ + "/external-test-data/general-real.mtx"
                  let actualResult = MatrixReader(fp).Read(parseFloat)

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
                  let fp = __SOURCE_DIRECTORY__ + "/external-test-data/general-real.mtx"
                  let unitMapping _ = () // If i pass an anonymous function such as (fun _ -> ()) then Linter will cause an error
                  let actualResult = MatrixReader(fp).Read(unitMapping)

                  let expectedResult =
                      let data = [ (0u, 0u, ()); (4u, 0u, ()); (5u, 0u, ()); (6u, 0u, ()); (7u, 0u, ()) ]
                      COOMatrix(data, 8u, 8u) |> SparseMatrix

                  Expect.equal actualResult.Data expectedResult.Data "Matrices don't match"

              testCase "General integer value matrix"
              <| fun _ ->
                  let fp = __SOURCE_DIRECTORY__ + "/external-test-data/general-integer.mtx"
                  let actualResult = MatrixReader(fp).Read(parseInt)

                  let expectedResult =
                      let data = [ (0u, 0u, 2); (4u, 0u, 1); (5u, 0u, 20); (6u, 0u, -3); (7u, 0u, 1) ]
                      COOMatrix(data, 8u, 8u) |> SparseMatrix

                  Expect.equal actualResult.Data expectedResult.Data "Matrices don't match" ]
