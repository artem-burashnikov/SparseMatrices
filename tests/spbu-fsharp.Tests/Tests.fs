namespace spbu_fsharp.Tests

open Expecto
open spbu_fsharp.Main

module EqualityTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Test for sum"
              <| fun _ ->
                  let actualResult = pow 2 10
                  Expect.equal actualResult 1024 "Results don't match" ]
