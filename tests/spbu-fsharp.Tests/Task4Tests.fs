module Task4Tests

open System
open System.Collections.Generic
open Helpers.Numbers
open HomeWork4.VectorData
open Expecto
open FsCheck
open Microsoft.FSharp.Core
open Trees

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
            <| fun (arr: array<_>) ->
                let expectedLeft, expectedRight = Array.splitAt ((ceilPowTwo arr.Length)/2) arr
                let actualLeft, actualRight = vecPartition (Vector(arr, 0, (ceilPowTwo arr.Length) - 1))
                Expect.sequenceEqual actualLeft.Data[actualLeft.Left..actualLeft.Right] expectedLeft ""
                Expect.sequenceEqual actualRight.Data[actualRight.Left..actualRight.Right] expectedRight ""

            testProperty "Partitioning then concatenating should output the initial array"
            <| fun (arr: array<_>) ->
                let leftPart, rightPart = vecPartition (Vector(arr, 0, (ceilPowTwo arr.Length) - 1))
                let actualResult = Array.concat [leftPart.Data[leftPart.Left..leftPart.Right]; rightPart.Data[rightPart.Left..rightPart.Right]]
                Expect.sequenceEqual actualResult arr ""

            testProperty "Array to Binary Tree converter: The set of original data must not change"
            <| fun (arr: array<_>) ->

                let folder (acc: HashSet<'value>) x =
                        acc.Add(x) |> ignore
                        acc

                // Skipping this test because BinaryTrees.fold produces different result by design.
                if arr.Length = 0 then
                    skiptest ""
                else
                    let tree = vecToTree arr
                    let actualResult = BinTrees.fold folder (HashSet()) tree
                    let expectedResult = Array.fold folder (HashSet()) arr
                    Expect.equal (expectedResult.IsSubsetOf actualResult && actualResult.IsSubsetOf expectedResult) true "Did not produce equal sets."

            testCase "vecToTree: empty array should produce an empty tree"
            <| fun _ ->
                let actualResult = vecToTree [||]
                let expectedResult = BinTrees.None
                Expect.equal actualResult expectedResult ""

            testCase "vecToTree: Simplest 2-elements array (different values)"
            <| fun _ ->
                let actualResult = vecToTree [|Some 1; Some 2|]
                let expectedResult = BinTrees.Node(BinTrees.Leaf 1, BinTrees.Leaf 2)
                Expect.equal actualResult expectedResult ""

            testCase "vecToTree: Simplest 2-elements array (identical values)"
            <| fun _ ->
                let actualResult = vecToTree [|Some 1; Some 1|]
                let expectedResult = BinTrees.Leaf(1)
                Expect.equal actualResult expectedResult ""
    ]

