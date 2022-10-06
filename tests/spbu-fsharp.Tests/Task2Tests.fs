module Task2Tests

open Lists
open Homework2
open Expecto
open FsCheck
open Microsoft.FSharp.Core

type MyComparableValues =
    | Ints of int
    | Booleans of bool
    // This type fails with Cons(Floats nan, Empty)
    // | Floats of float
    | Strings of string
    | Chars of char
    | Bytes of byte

module TestCases =

    let config = { Config.Default with MaxTest = 10000 }

    [<Tests>]
    let tests =

        testList "samples" [

            testProperty "List -> MyList -> List should return the original list"
                <| fun lst ->
                    let result = myListToList (listToMyList lst)
                    Expect.equal lst result "The results were different"

            testProperty "List -> MyOOPList -> List should return the original list"
                <| fun lst ->
                    let result = myOOPListToList (listToMyOOPList lst)
                    Expect.equal lst result "The results were different"

            testProperty "List -> MyOOPList -> MyList -> MyOOPList -> List should return the original list"
                <| fun lst ->
                    let actualResult = myOOPListToList (myListToMyOOPList <| (myOOPListToMyList <| listToMyOOPList lst))
                    Expect.equal lst actualResult "The results were different"

            testProperty "List -> MyList -> MyOOPList -> MyList -> List should return the original list"
                <| fun lst ->
                    let actualResult = myListToList (myOOPListToMyList <| (myListToMyOOPList <| listToMyList lst))
                    Expect.equal lst actualResult "The results were different"

            testProperty "BubbleSort (MyList): Sorting algorithms should produce the same result"
                <| fun (myList: MyList<MyComparableValues>) ->
                    let expectedResult = List.sort <| myListToList myList
                    let actualResult = myListToList <| MyLists.bubbleSort myList
                    Expect.equal expectedResult actualResult "The results were different"

            testProperty "Quicksort (MyList): Sorting algorithms should produce the same result"
                <| fun (myList: MyList<MyComparableValues>) ->
                    let expectedResult = List.sort <| myListToList myList
                    let actualResult = myListToList <| MyLists.qSort myList
                    Expect.equal expectedResult actualResult "The results were different"

            testProperty "BubbleSort (MyOOPList) Sorting algorithms should produce the same result"
                <| fun (lst: list<MyComparableValues>) ->
                    let expectedResult = List.sort lst
                    let actualResult = myOOPListToList (MyOOPLists.bubbleSort <| listToMyOOPList lst)
                    Expect.equal expectedResult actualResult "The results were different"

            testProperty "QuickSort (MyOOPList) Sorting algorithms should produce the same result"
                <| fun (myList: MyList<MyComparableValues>) ->
                    let expectedResult = List.sort <| myListToList myList
                    let actualResult = myOOPListToList (MyOOPLists.qSort <| myListToMyOOPList myList)
                    Expect.equal expectedResult actualResult "The results were different"

            testCase "If both Empty, result should be Empty (MyList)"
                <| fun _ ->
                    let expectedResult = []
                    let actualResult = myListToList <| MyLists.concat Empty Empty
                    Expect.equal expectedResult actualResult "The result must be empty if both are empty"

            testCase "If both Empty, result should be Empty (MyOOPList)"
                <| fun _ ->
                    let expectedResult = []
                    let actualResult = myOOPListToList <| MyOOPLists.concat (MyOOPEmptyList()) (MyOOPEmptyList())
                    Expect.equal expectedResult actualResult "The result must be empty if both are empty"

            testProperty "Resulting length should be the sum of initial lengths (MyList)"
                <| fun myList1 myList2 ->
                    let lengthOfCats = getLength (MyLists.concat myList1 myList2)
                    let sumOfCats = (getLength myList1) + (getLength myList2)
                    Expect.equal lengthOfCats sumOfCats "Lengths must match"

            testProperty "Resulting length should be the sum of initial lengths (MyOOPList)"
                <| fun lst1 lst2 ->
                    let lengthOfCats = List.length (myOOPListToList <| MyOOPLists.concat (listToMyOOPList lst1) (listToMyOOPList lst2))
                    let sumOfCats = getLengthOOP (listToMyOOPList lst1)  + getLengthOOP (listToMyOOPList lst2)
                    Expect.equal lengthOfCats sumOfCats "Lengths must match"

            // // This test produces System.Incomparable error.
            // testProperty "Test with an Error"
            //     <| fun lst ->
            //         let expectedResult = List.sort lst
            //         let actualResult = myListToList (MyLists.qSort <| listToMyList lst)
            //         Expect.equal expectedResult actualResult "The results were different"

            testCase "Empty + something should be something (MyList)"
                <| fun _ ->
                    let actualResult = MyLists.concat Empty (Cons(2, Cons(2, Empty)))
                    Expect.equal actualResult (Cons(2, Cons(2, Empty))) "The result should be a non-empty argument."

            testCase "Empty + something should be something (MyOOPList)"
                <| fun _ ->
                    let lst = MyOOPNonEmptyList(4, MyOOPNonEmptyList(3, MyOOPNonEmptyList(2, MyOOPNonEmptyList(2, MyOOPEmptyList()))))
                    let actualResult = MyOOPLists.concat (MyOOPEmptyList()) lst
                    Expect.equal actualResult lst "The result should be a non-empty argument."

]
