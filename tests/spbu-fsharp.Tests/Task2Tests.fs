module Task2Tests

open Lists
open HomeWork2
open Expecto
open FsCheck
open Microsoft.FSharp.Core

// This type is used for testing sorting functions
// which require list's values to be numeric.
type MyComparableNumType =
    | Ints of int
    | Floats of float

module TestCases =

    let config = { Config.Default with MaxTest = 10000 }

    [<Tests>]
    let tests =

        testList "samples" [

            testProperty "Sorting algorithms should produce the same result"
                <| fun (myList: MyList<MyComparableNumType>) ->
                    let sort1 = bubbleSort myList
                    let sort2 = qSort myList
                    Expect.equal (checkEqual sort1 sort2) true

            testCase "If both Empty, result should be Empty"
                <| fun _ ->
                    let actualResult = concat Empty Empty
                    Expect.equal actualResult Empty "Should be empty"

            testProperty "Resulting length should be the sum of initial lengths"
                <| fun myList1 myList2 ->
                    let lengthOfCat = getLength (concat myList1 myList2)
                    let sumOfLengths = (getLength myList1) + (getLength myList2)
                    Expect.equal lengthOfCat sumOfLengths "Lengths must match"

            testCase "Empty + something should be something"
                <| fun _ ->
                    let actualResult = concat Empty (Cons(2, Cons(2, Empty)))
                    Expect.equal actualResult (Cons(2, Cons(2, Empty))) "failed to match"

]
