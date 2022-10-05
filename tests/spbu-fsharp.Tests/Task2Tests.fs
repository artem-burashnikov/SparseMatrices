module Task2Tests

open Lists
open HomeWork2
open Expecto
open FsCheck
open Microsoft.FSharp.Core



module TestCases =

    let config = { Config.Default with MaxTest = 10000 }

    type MyComparableValues =
        | Ints of int
        | Floats of float
        | Strings of string
        | Chars of char
        | Bytes of byte

    [<Tests>]
    let tests =

        testList "samples" [

            // TODO Make a custom type generator
            testProperty "Sorting algorithms should produce the same result (MyList)"
                <| fun (myList:MyList<MyComparableValues>) ->
                    let sort1 = bubbleSort myList
                    let sort2 = qSort myList
                    Expect.equal true <| checkEqual sort1 sort2 <| "The result were different"

            // TODO Make a custom type equality checker
            // TODO Make a custom type generator
            testCase "Sorting algorithms should produce the same result (MyOOPList)"
                <| fun _ ->
                    let lst = MyOOPNonEmptyList(4, MyOOPNonEmptyList(3, MyOOPNonEmptyList(2, MyOOPNonEmptyList(2, MyOOPEmptyList()))))
                    let sort1 = lst |> bubbleSortOOP |> toMyList
                    let sort2 = lst |> qSortOOP |> toMyList
                    Expect.equal true <| checkEqual sort1 sort2 <| "The result were different"

            testCase "If both Empty, result should be Empty (MyList)"
                <| fun _ ->
                    let actualResult = concat Empty Empty
                    Expect.equal actualResult Empty "Should be empty"

            testCase "If both Empty, result should be Empty (MyOOPList)"
                <| fun _ ->
                    let actualResult = concatOOP (MyOOPEmptyList()) (MyOOPEmptyList())
                    Expect.equal actualResult (MyOOPEmptyList()) "Should be empty"

            testProperty "Resulting length should be the sum of initial lengths (MyList)"
                <| fun (myList1:MyList<_>) (myList2:MyList<_>) ->
                    let lengthOfCat = getLength (concat myList1 myList2)
                    let sumOfLengths = (getLength myList1) + (getLength myList2)
                    Expect.equal lengthOfCat sumOfLengths "Lengths must match"

            testProperty "Resulting length should be the sum of initial lengths (MyOOPList)"
                <| fun myOOPList1 myOOPList2 ->
                    let lengthOfCat = getLengthOOP (concatOOP myOOPList1 myOOPList2)
                    let sumOfLengths = (getLengthOOP myOOPList1) + (getLengthOOP myOOPList2)
                    Expect.equal lengthOfCat sumOfLengths "Lengths must match"

            testCase "Empty + something should be something (MyList)"
                <| fun _ ->
                    let actualResult = concat Empty (Cons(2, Cons(2, Empty)))
                    Expect.equal actualResult (Cons(2, Cons(2, Empty))) "failed to match"

            testCase "Empty + something should be something (MyOOPList)"
                <| fun _ ->
                    let lst = MyOOPNonEmptyList(4, MyOOPNonEmptyList(3, MyOOPNonEmptyList(2, MyOOPNonEmptyList(2, MyOOPEmptyList()))))
                    let actualResult = concatOOP (MyOOPEmptyList()) lst
                    Expect.equal actualResult lst "failed to match"

]
