module Task4Tests

open Expecto
open FsCheck
open Microsoft.FSharp.Core

module TestCases =

    let config = { Config.Default with MaxTest = 10000 }

    [<Tests>]
    let tests =

        testList
            "samples" [

              testCase "Temp"
              <| fun _ ->
                  let result = true
                  Expect.equal result true "Failed to match"
]

