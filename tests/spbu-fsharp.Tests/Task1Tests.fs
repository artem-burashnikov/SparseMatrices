module Task1Tests

open Expecto.Flip
open HomeWork1
open Expecto
open Microsoft.FSharp.Core

module TestCases =

    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    [<Tests>]
    let tests =

        testList "samples" [

            testPropertyWithConfig config "pow: All power functions should return equal results"
                <| fun (x: int) (y: int) ->
                    if x <> 0 && y <> 0 then
                        qPow x y = pown x y
                    else
                        skiptest "incorrect arguments"

            testProperty "pow: Bigger exponent should result in a greater result"
                <| fun arg exp1 exp2 ->
                    if exp1 > exp2 && arg > 1 && exp1 > 1 && exp2 > 1 then
                        qPow arg exp1 > qPow arg exp2
                    else
                        skiptest "incorrect arguments"

            testCase "pow: 0 to the 0th power is not defined"
                <| fun _ ->
                let actualResult =
                    Expect.throws (fun _ -> pow 0 0 |> ignore) "undefined"
                actualResult

            testProperty "qPow: All power functions should return equal results"
                <| fun (x: int) (y: int) ->
                    if x <> 0 && y <> 0 then
                        qPow x y = pown x y
                    else
                        skiptest "incorrect arguments"

            testProperty "qPow: Bigger exponent should result in a greater result"
                <| fun arg exp1 exp2 ->
                    if exp1 > exp2 && arg > 1 && exp1 > 1 && exp2 > 1 then
                        qPow arg exp1 > qPow arg exp2
                    else
                        skiptest "incorrect arguments"

            testCase "qPow: 0 to the 0th power is not defined"
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

            // testProperty "allOdds: assuming ascending order of the resulting array, the first element is determined by input"
            //     <| fun a b ->
            //         let smallerNum = min a b
            //         let actualResult = allOdds a b
            //         if Array.length <| actualResult <> 0 then
            //             if a % 2 = 0 then
            //                 Expect.equal (smallerNum + 1) actualResult[0]
            //             else
            //                 Expect.equal (smallerNum + 2) actualResult[0]
            //         else
            //             skiptest "incorrect array"

            testCase "allOdds: The same number is given twice"
                <| fun _ ->
                let actualResult = allOdds 5 5
                Expect.equal actualResult [||] "Results don't match"
]
