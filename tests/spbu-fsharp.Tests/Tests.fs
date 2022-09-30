namespace spbu_fsharp.Tests

open Expecto
open ExpectoFsCheck
open FsCheck
open Microsoft.FSharp.Core
open spbu_fsharp
open spbu_fsharp.Main.HomeWork1
open spbu_fsharp.Main.HomeWork2

module FsCheckTests =

    // Is this being used?
    let config = { Config.Default with MaxTest = 10000 }

    [<Tests>]
    let tests =

        testList
            "FsCheck tests"
            [ testList
                  "Naive power function"
                  [ testProperty "All power functions should return equal results"
                    <| fun (x: int) (y: int) ->
                        if x <> 0 && y <> 0 then
                            qPow x y = pown x y
                        else
                            skiptest "incorrect arguments"

                    testProperty "Bigger exponent should result in a greater result"
                    <| fun arg exp1 exp2 ->
                        if exp1 > exp2 && arg > 1 && exp1 > 1 && exp2 > 1 then
                            qPow arg exp1 > qPow arg exp2
                        else
                            skiptest "incorrect arguments"

                    // I have slight idea why it works.
                    testCase "1.6: pow. 0 to the 0th power is not defined"
                    <| fun _ ->
                        let actualResult = Expect.throws (fun _ -> pow 0 0 |> ignore) "undefined"

                        actualResult ]
              testList
                  "Fast power function"
                  [ testProperty "All power functions should return equal results"
                    <| fun (x: int) (y: int) ->
                        if x <> 0 && y <> 0 then
                            qPow x y = pown x y
                        else
                            skiptest "incorrect arguments"

                    testProperty "Bigger exponent should result in a greater result"
                    <| fun arg exp1 exp2 ->
                        if exp1 > exp2 && arg > 1 && exp1 > 1 && exp2 > 1 then
                            qPow arg exp1 > qPow arg exp2
                        else
                            skiptest "incorrect arguments"

                    testCase "2.5: pow. 0 to the 0th power is not defined"
                    <| fun _ ->
                        let actualResult = Expect.throws (fun _ -> qPow 0 0 |> ignore) "undefined"

                        actualResult ]
              testList
                  "Difference"
                  [ testCase "3.1: diff. All numbers are the same"
                    <| fun _ ->
                        let actualResult = diff [| 0; 0; 0; 0; 0 |]
                        Expect.equal actualResult 0 "Results don't match"

                    testCase "3.2: diff. All numbers are negative."
                    <| fun _ ->
                        let actualResult = diff [| -1; -2; -3; -4; -5 |]

                        Expect.equal actualResult 4 "Results don't match"

                    testCase "3.3: diff. All numbers are positive."
                    <| fun _ ->
                        let actualResult = diff [| 1; 2; 3; 4; 5 |]
                        Expect.equal actualResult 4 "Results don't match"

                    testCase "3.4: diff. Positive and negative numbers."
                    <| fun _ ->
                        let actualResult = diff [| 1; 2; -3; -4; -5 |]

                        Expect.equal actualResult 7 "Results don't match"

                    testCase "3.5: diff. An array only has one element"
                    <| fun _ ->
                        let actualResult = diff [| 5 |]
                        Expect.equal actualResult 0 "Results don't match"

                    testCase "3.6: diff. An empty array is given"
                    <| fun _ ->
                        let actualResult = Expect.throws (fun _ -> diff [||] |> ignore) "undefined"

                        actualResult ]
              testList
                  "Odds in between"
                  [ testCase "4.1: all_ods. In between two positive integers"
                    <| fun _ ->
                        let actualResult = allOdds 1 10
                        Expect.equal actualResult [| 3; 5; 7; 9 |] "Results don't match"

                    testCase "4.2: all_ods. In between two negative integers"
                    <| fun _ ->
                        let actualResult = allOdds -10 -1
                        Expect.equal actualResult [| -9; -7; -5; -3 |] "Results don't match"

                    testCase "4.3: all_ods. In between a positive and a negative integer"
                    <| fun _ ->
                        let actualResult = allOdds -5 5
                        Expect.equal actualResult [| -3; -1; 1; 3 |] "Results don't match"

                    testCase "4.4: all_ods. The same number is given twice"
                    <| fun _ ->
                        let actualResult = allOdds 5 5
                        Expect.equal actualResult [||] "Results don't match"



                    ] ]
