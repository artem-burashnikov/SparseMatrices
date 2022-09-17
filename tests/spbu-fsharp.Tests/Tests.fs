namespace spbu_fsharp.Tests

open Expecto
open spbu_fsharp

module SayTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Test for sum"
              <| fun _ ->
                  let actualResult = spbu_fsharp.Main.sum 10 0
                  Expect.equal actualResult 55 "Not an absolute unit" ]
// testCase "Say hello all"
// <| fun _ ->
//     let subject = Say.hello "all"
//     Expect.equal subject "Hello all" "You didn't say hello" ]
