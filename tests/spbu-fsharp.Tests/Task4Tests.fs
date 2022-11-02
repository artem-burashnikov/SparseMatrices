module Task4Tests

open System
open Helpers.Numbers
open HomeWork4
open Expecto
open FsCheck
open Microsoft.FSharp.Core

module TestCases =

    let config = { Config.Default with MaxTest = 10000 }

    [<Tests>]
    let tests =

        testList
            "samples" [

            testCase "ceilPowTwo: Input is equal to 0."
            <| fun _ ->
                let actualResult = ceilPowTwo 0
                let expectedResult = 1
                Expect.equal actualResult expectedResult "Incorrect output."

            testCase "ceilPowTwo: Negative input."
            <| fun _ ->
                let actualResult = ceilPowTwo -10
                let expectedResult = 1
                Expect.equal actualResult expectedResult "Incorrect output."

            testCase "ceilPowTwo: Positive input."
            <| fun _ ->
                let actualResult = ceilPowTwo 10
                let expectedResult = 16
                Expect.equal actualResult expectedResult "Incorrect output."

            testProperty "ceilPowTwo: Output should be no less than a given input."
            <| fun x ->
                let actualResult = ceilPowTwo x
                actualResult >= x

            testProperty "ceilPowTwo: Different function that does the same should produce the same result."
            <| fun x ->
                let result1 = ceilPowTwo x
                let result2 =
                    if x <= 0 then
                        1
                    elif x = 1 then
                        2
                    else
                        pown 2 (int << ceil <| Math.Log2(double x))
                result1 = result2

            testProperty "Vector partitioning: built-in function should produce the same result."
            <| fun (arr: array<int option>) ->
                let expectedLeft, expectedRight = Array.splitAt ((ceilPowTwo arr.Length)/2) arr
                let actualLeft, actualRight = vecPartition (Vector(arr, 0, (ceilPowTwo arr.Length) - 1))
                Expect.sequenceEqual actualLeft.Data[actualLeft.Left..actualLeft.Right] expectedLeft ""
                Expect.sequenceEqual actualRight.Data[actualRight.Left..actualRight.Right] expectedRight ""
]

