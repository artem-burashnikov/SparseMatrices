module Task4Tests

open System
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

            testProperty "ceilPowTwo: Different function should produce the same result."
            <| fun x ->
                let result1 = ceilPowTwo x
                let result2 =
                    if x <= 0 then
                        1
                    else
                        pown 2 (int << ceil <| Math.Log2(double x))
                result1 = result2
]

