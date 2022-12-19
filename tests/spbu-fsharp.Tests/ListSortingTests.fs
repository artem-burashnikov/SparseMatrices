module ListSortingTests

open CLists
open OOPLists
open Helpers.ListConverters
open Expecto
open FsCheck
open Microsoft.FSharp.Core

module TestCases =

    let config = { Config.Default with MaxTest = 10000 }

    [<Tests>]
    let tests =

        testList
            "samples"
            [

              testProperty "List -> MyList -> List should return the original list"
              <| fun lst ->
                  let result = listToLst (lstToList lst)
                  Expect.equal lst result "The results were different"

              testProperty "List -> MyOOPLists -> List should return the original list"
              <| fun lst ->
                  let result = oopToLst (lstToOOP lst)
                  Expect.equal lst result "The results were different"

              testProperty "List -> MyOOPLists -> MyList -> MyOOPLists -> List should return the original list"
              <| fun lst ->

                  let results = Array.zeroCreate 10

                  for i = 0 to 9 do
                      let result = oopToLst (listToOOP <| (oopToList <| lstToOOP lst))

                      if result = lst then
                          results[i] <- true
                      else
                          results[i] <- false

                  let allMatch = Array.forall id

                  Expect.equal true (allMatch results) "The results were different"

              testProperty "List -> MyList -> MyOOPLists -> MyList -> List should return the original list"
              <| fun lst ->
                  let actualResult = listToLst (oopToList <| (listToOOP <| lstToList lst))
                  Expect.equal lst actualResult "The results were different"

              testProperty "BubbleSort int (MyList): Sorting algorithms should produce the same result"
              <| fun (lst: list<int>) ->
                  let expectedResult = List.sort lst
                  let actualResult = listToLst (CLists.bubbleSort <| lstToList lst)
                  Expect.equal expectedResult actualResult "The results were different"

              testProperty "BubbleSort string (MyList): Sorting algorithms should produce the same result"
              <| fun (lst: list<string>) ->
                  let expectedResult = List.sort lst
                  let actualResult = listToLst (CLists.bubbleSort <| lstToList lst)
                  Expect.equal expectedResult actualResult "The results were different"

              testProperty "Quicksort int (MyList): Sorting algorithms should produce the same result"
              <| fun (lst: list<int>) ->
                  let expectedResult = List.sort lst
                  let actualResult = listToLst (CLists.qSort <| lstToList lst)
                  Expect.equal expectedResult actualResult "The results were different"

              testProperty "Quicksort string (MyList): Sorting algorithms should produce the same result"
              <| fun (lst: list<string>) ->
                  let expectedResult = List.sort lst
                  let actualResult = listToLst (CLists.qSort <| lstToList lst)
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyList): Edge case of an empty list"
              <| fun _ ->
                  let expectedResult = Empty
                  let actualResult = CLists.bubbleSort Empty
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyList): Edge case of a single element"
              <| fun _ ->
                  let expectedResult = Cons(0, Empty)
                  let actualResult = CLists.bubbleSort (Cons(0, Empty))
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyList): Edge case of a two identical elements"
              <| fun _ ->
                  let expectedResult = Cons(0, Cons(0, Empty))
                  let actualResult = CLists.bubbleSort (Cons(0, Cons(0, Empty)))
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyList): Edge case of two different elements (unsorted)"
              <| fun _ ->
                  let expectedResult = Cons(0, Cons(1, Empty))
                  let actualResult = CLists.bubbleSort (Cons(1, Cons(0, Empty)))
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyList): Edge case of two different elements (already sorted)"
              <| fun _ ->
                  let expectedResult = Cons(0, Cons(1, Empty))
                  let actualResult = CLists.bubbleSort (Cons(0, Cons(1, Empty)))
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyList): Edge case of an empty list"
              <| fun _ ->
                  let expectedResult = Empty
                  let actualResult = CLists.qSort Empty
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyList): Edge case of a single element"
              <| fun _ ->
                  let expectedResult = Cons(0, Empty)
                  let actualResult = CLists.qSort (Cons(0, Empty))
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyList): Edge case of a two identical elements"
              <| fun _ ->
                  let expectedResult = Cons(0, Cons(0, Empty))
                  let actualResult = CLists.qSort (Cons(0, Cons(0, Empty)))
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyList): Edge case of two different elements (unsorted)"
              <| fun _ ->
                  let expectedResult = Cons(0, Cons(1, Empty))
                  let actualResult = CLists.qSort (Cons(1, Cons(0, Empty)))
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyList): Edge case of two different elements (already sorted)"
              <| fun _ ->
                  let expectedResult = Cons(0, Cons(1, Empty))
                  let actualResult = CLists.qSort (Cons(0, Cons(1, Empty)))
                  Expect.equal expectedResult actualResult "The results were different"

              testProperty "BubbleSort int (MyOOPLists) Sorting algorithms should produce the same result"
              <| fun (lst: list<int>) ->
                  let expectedResult = List.sort lst
                  let actualResult = oopToLst (bubbleSort <| lstToOOP lst)
                  Expect.equal expectedResult actualResult "The results were different"

              testProperty "BubbleSort string (MyOOPLists) Sorting algorithms should produce the same result"
              <| fun (lst: list<string>) ->
                  let expectedResult = List.sort lst
                  let actualResult = oopToLst (bubbleSort <| lstToOOP lst)
                  Expect.equal expectedResult actualResult "The results were different"

              testProperty "QuickSort int (MyOOPLists) Sorting algorithms should produce the same result"
              <| fun (lst: list<int>) ->
                  let expectedResult = List.sort lst
                  let actualResult = oopToLst (qSort <| lstToOOP lst)
                  Expect.equal expectedResult actualResult "The results were different"

              testProperty "QuickSort string (MyOOPLists) Sorting algorithms should produce the same result"
              <| fun (lst: list<string>) ->
                  let expectedResult = List.sort lst
                  let actualResult = oopToLst (qSort <| lstToOOP lst)
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyOOPLists): Edge case of an empty list"
              <| fun _ ->
                  let expectedResult = oopToLst <| EmptyList()
                  let actualResult = oopToLst << bubbleSort <| EmptyList()
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyOOPLists): Edge case of a single element"
              <| fun _ ->
                  let expectedResult = oopToLst <| List(0, EmptyList())

                  let actualResult = oopToLst << bubbleSort <| List(0, EmptyList())

                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyOOPLists): Edge case of a two identical elements"
              <| fun _ ->
                  let expectedResult = oopToLst <| List(0, List(0, EmptyList()))

                  let actualResult = oopToLst << bubbleSort <| List(0, List(0, EmptyList()))

                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyOOPLists): Edge case of two different elements (unsorted)"
              <| fun _ ->
                  let expectedResult = oopToLst <| List(0, List(1, EmptyList()))

                  let actualResult = oopToLst << bubbleSort <| List(1, List(0, EmptyList()))

                  Expect.equal expectedResult actualResult "The results were different"

              testCase "BubbleSort (MyOOPLists): Edge case of two different elements (already sorted)"
              <| fun _ ->
                  let expectedResult = oopToLst <| List(0, List(1, EmptyList()))

                  let actualResult = oopToLst << bubbleSort <| List(0, List(1, EmptyList()))

                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyOOPLists): Edge case of an empty list"
              <| fun _ ->
                  let expectedResult = oopToLst <| EmptyList()
                  let actualResult = oopToLst << qSort <| EmptyList()
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyOOPLists): Edge case of a single element"
              <| fun _ ->
                  let expectedResult = oopToLst <| List(0, EmptyList())
                  let actualResult = oopToLst << qSort <| List(0, EmptyList())
                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyOOPLists): Edge case of a two identical elements"
              <| fun _ ->
                  let expectedResult = oopToLst <| List(0, List(0, EmptyList()))

                  let actualResult = oopToLst << qSort <| List(0, List(0, EmptyList()))

                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyOOPLists): Edge case of two different elements (unsorted)"
              <| fun _ ->
                  let expectedResult = oopToLst <| List(0, List(1, EmptyList()))

                  let actualResult = oopToLst << qSort <| List(0, List(1, EmptyList()))

                  Expect.equal expectedResult actualResult "The results were different"

              testCase "QuickSort (MyOOPLists): Edge case of two different elements (already sorted)"
              <| fun _ ->
                  let expectedResult = oopToLst <| List(0, List(1, EmptyList()))

                  let actualResult = oopToLst << qSort <| List(0, List(1, EmptyList()))

                  Expect.equal expectedResult actualResult "The results were different"

              testCase "If both Empty, result should be Empty (MyList)"
              <| fun _ ->
                  let expectedResult = []
                  let actualResult = listToLst <| CLists.concat Empty Empty
                  Expect.equal expectedResult actualResult "The result must be empty if both are empty"

              testCase "If both Empty, result should be Empty (MyOOPLists)"
              <| fun _ ->
                  let expectedResult = []

                  let actualResult = oopToLst <| concat (EmptyList()) (EmptyList())

                  Expect.equal expectedResult actualResult "The result must be empty if both are empty"

              testProperty "Resulting length should be the sum of initial lengths (MyList)"
              <| fun myList1 myList2 ->
                  let lengthOfCats = CLists.getLength (CLists.concat myList1 myList2)

                  let sumOfCats = (CLists.getLength myList1) + (CLists.getLength myList2)

                  Expect.equal lengthOfCats sumOfCats "Lengths must match"

              testProperty "Resulting length should be the sum of initial lengths (MyOOPLists)"
              <| fun lst1 lst2 ->
                  let lengthOfCats = List.length << oopToLst <| concat (lstToOOP lst1) (lstToOOP lst2)

                  let sumOfCats = (getLength <| lstToOOP lst1) + (getLength <| lstToOOP lst2)

                  Expect.equal lengthOfCats sumOfCats "Lengths must match"

              testCase "Empty + something should be something (MyList)"
              <| fun _ ->
                  let actualResult = CLists.concat Empty (Cons(2, Cons(2, Empty)))
                  Expect.equal actualResult (Cons(2, Cons(2, Empty))) "The result should be a non-empty argument."

              testCase "Empty + something should be something (MyOOPLists)"
              <| fun _ ->
                  let lst = List(4, List(3, List(2, List(2, EmptyList()))))
                  let actualResult = concat (EmptyList()) lst
                  Expect.equal actualResult lst "The result should be a non-empty argument."

              ]
