module BasicSyntaxTests

open Expecto.Flip
open BasicSyntax
open Expecto
open Microsoft.FSharp.Core

module TestCases =

    let config =
        { FsCheckConfig.defaultConfig with
            maxTest = 10000 }

    [<Tests>]
    let tests =

        testList
            "samples"
            [ testCase "qPow: 0 to the 0th power is not defined"
              <| fun _ ->
                  let actualResult = Expect.throws (fun _ -> qPow 0 0 |> ignore) "undefined"
                  actualResult

              testCase "diff: All numbers in the array are negative."
              <| fun _ ->
                  let actualResult = diff [| -1; -2; -3; -4; -5 |]
                  Expect.equal actualResult 4 "Results don't match"

              testCase "diff: All numbers in the array are positive."
              <| fun _ ->
                  let actualResult = diff [| 1; 2; 3; 4; 5 |]
                  Expect.equal actualResult 4 "Results don't match"

              testCase "diff: An array only has one element"
              <| fun _ ->
                  let actualResult = diff [| 5 |]
                  Expect.equal actualResult 0 "Results don't match"

              testCase "An empty array is given"
              <| fun _ ->
                  let actualResult = Expect.throws (fun _ -> diff [||] |> ignore) "undefined"
                  actualResult

              testCase "allOdds: In between two positive integers"
              <| fun _ ->
                  let actualResult = allOdds 1 10
                  Expect.equal actualResult [| 3; 5; 7; 9 |] "Results don't match"

              testCase "allOdds: The same number is given twice"
              <| fun _ ->
                  let actualResult = allOdds 5 5
                  Expect.equal actualResult [||] "Results don't match" ]
