module Task4Tests

open System
open Helpers.Numbers
open HomeWork4
open HomeWork4.MatrixData
open HomeWork4.SparseVector
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
                  if x <= 0 then 1
                  elif x = 1 then 2
                  else pown 2 (int << ceil <| Math.Log2(double x))

              result1 = result2

            testProperty "Vector partitioning: built-in function should produce the same result."
            <| fun (arr: array<_>) ->
              let expectedLeft, expectedRight = Array.splitAt ((ceilPowTwo arr.Length) / 2) arr

              let actualLeft, actualRight =
                  vecPartition (Vector(arr, 0, (ceilPowTwo arr.Length) - 1))

              Expect.sequenceEqual actualLeft.Data[actualLeft.Left .. actualLeft.Right] expectedLeft ""
              Expect.sequenceEqual actualRight.Data[actualRight.Left .. actualRight.Right] expectedRight ""

            testProperty "Partitioning then concatenating should output the initial array"
            <| fun (arr: array<_>) ->
              let leftPart, rightPart = vecPartition (Vector(arr, 0, (ceilPowTwo arr.Length) - 1))

              let actualResult =
                  Array.concat
                      [ leftPart.Data[leftPart.Left .. leftPart.Right]
                        rightPart.Data[rightPart.Left .. rightPart.Right] ]

              Expect.sequenceEqual actualResult arr ""

            testCase "vecToTree: empty array should produce an empty tree"
            <| fun _ ->
              let actualResult = vecToTree [||]
              let expectedResult = BinTrees.None
              Expect.equal actualResult expectedResult ""

            testCase "vecToTree: 1-element array"
            <| fun _ ->
              let actualResult = vecToTree [| Some 1 |]
              let expectedResult = BinTrees.Leaf 1
              Expect.equal actualResult expectedResult ""

            testCase "vecToTree: 2-elements array (different values)"
            <| fun _ ->
              let actualResult = vecToTree [| Some 1; Some 2 |]
              let expectedResult = BinTrees.Node(BinTrees.Leaf 1, BinTrees.Leaf 2)
              Expect.equal actualResult expectedResult ""

            testCase "vecToTree: 2-elements array (identical values)"
            <| fun _ ->
              let actualResult = vecToTree [| Some 1; Some 1 |]
              let expectedResult = BinTrees.Leaf(1)
              Expect.equal actualResult expectedResult ""

            testCase "vecToTree: 3-elements array (different values)"
            <| fun _ ->
              let actualResult = vecToTree [| Some 1; None; Some 2 |]

              let expectedResult =
                  BinTrees.Node(
                      BinTrees.Node(BinTrees.Leaf 1, BinTrees.None),
                      BinTrees.Node(BinTrees.Leaf 2, BinTrees.None)
                  )

              Expect.equal actualResult expectedResult ""

            testProperty
              "SparseVector.GetValue loop through each index collecting values. Resulting sequence should be equal to the original."
            <| fun (arr: array<_>) ->
              let sparseVec = toSparse arr
              let mutable actualResult = []

              for i = 1 to arr.Length do
                  actualResult <- sparseVec[i] :: actualResult

              Expect.sequenceEqual (actualResult |> List.rev) arr ""

            testCase "Table partition: empty input"
            <| fun _ ->
                let rows = 0
                let cols = 0
                let mtx = Matrix([||], rows, cols, -1, -1, -1, -1)
                let nw, ne, sw, se = mtxPartition mtx
                Expect.equal nw.Data [||] ""
                Expect.equal ne.Data [||] ""
                Expect.equal sw.Data [||] ""
                Expect.equal se.Data [||] ""

            testProperty "Table partition (by indices)"
            <| fun (x: int) (y: int) ->

                let rows = abs x
                let cols = abs y

                if rows = 0 || cols = 0 then
                    skiptest "Table dimensions must be greater than 0"
                else

                    let paddedIndex = ceilPowTwo (max rows cols) - 1

                    let middle = paddedIndex / 2

                    // The data is not important, leaving empty for convenience.
                    let mtx = Matrix([||], rows, cols, 0, 0, paddedIndex, paddedIndex)

                    let nw, ne, sw, se = mtxPartition mtx
                    Expect.equal nw.Ix 0 ""
                    Expect.equal nw.Iy 0 ""
                    Expect.equal nw.Jx middle ""
                    Expect.equal nw.Jy middle ""
                    Expect.equal ne.Ix 0 ""
                    Expect.equal ne.Iy (middle + 1) ""
                    Expect.equal ne.Jx middle ""
                    Expect.equal ne.Jy paddedIndex ""
                    Expect.equal sw.Ix (middle + 1) ""
                    Expect.equal sw.Iy 0 ""
                    Expect.equal sw.Jx paddedIndex ""
                    Expect.equal sw.Jy middle ""
                    Expect.equal se.Ix (middle + 1) ""
                    Expect.equal se.Iy (middle + 1) ""
                    Expect.equal se.Jx paddedIndex ""
                    Expect.equal se.Jy paddedIndex ""
            ]
