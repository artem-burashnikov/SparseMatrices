namespace MatrixReaderTests

open Expecto
open Microsoft.FSharp.Core
open MatrixReader


module FileReading =

    [<Tests>]
    let tests =

        testList
            "File reading cases"
            [ testCase "Empty file"
              <| fun _ ->
                  let fp = __SOURCE_DIRECTORY__ + "external-data/empty-file"
                  Expect.throws (fun _ -> MatrixReader(fp) |> ignore) "Empty file was given"


              testCase "File is not empty, but is not a matrix market file"
              <| fun _ ->
                  let fp = __SOURCE_DIRECTORY__ + "/external-data/incorrect-file-type"

                  Expect.throws (fun _ -> MatrixReader(fp) |> ignore) "Not a matrix market file" ]
