module Task3Tests

open System.Collections.Generic
open CLists
open HomeWork3.NTrees
open Expecto
open FsCheck
open Microsoft.FSharp.Core

module TestCases =

    let config = { Config.Default with MaxTest = 10000 }

    [<Tests>]
    let tests =

        testList "samples" [

            testCase "A single source Node with no children"
                <| fun _ ->
                    let input = Node("abba", Empty)
                    let expectedResult = Cons("abba", Empty), 1
                    let actualResult = mkCList input, countUniques input
                    Expect.equal actualResult expectedResult "Failed to produce a correct list \
                        on a single source node."

            testCase "A single source Leaf with no children"
                <| fun _ ->
                    let input = Leaf("abba")
                    let expectedResult = Cons("abba", Empty), 1
                    let actualResult = mkCList input, countUniques input
                    Expect.equal actualResult expectedResult "Failed to produce a correct list \
                        on a single source node."

            testCase "A source Node which has a single child (values of both match)"
                <| fun _ ->
                    let input = Node("abba", Cons(Leaf("abba"), Empty))
                    let expectedResult = Cons("abba", Cons("abba", Empty)), 1
                    let actualResult = mkCList input, countUniques input
                    Expect.equal actualResult expectedResult "Failed to produce a correct list \
                        on a source node with a single child (values of both match)."

            testCase "A source Node which has a single child (values of both differ)"
                <| fun _ ->
                    let input = Node(1, Cons(Leaf(2), Empty))
                    let expectedResult = Cons(2, Cons(1, Empty)), 2
                    let actualResult = mkCList input, countUniques input
                    Expect.equal actualResult expectedResult "Failed to produce a correct list \
                        on a source node with a single child (values of both differ)."

            testCase "A source node with two children and a child's child (all values differ)"
                <| fun _ ->
                    let input = Node(1, Cons(Node(2, Cons(Leaf(3), Empty)), Cons(Leaf(4), Empty)))
                    let expectedResult = Cons(4, Cons(3, Cons(2, Cons(1, Empty)))), 4
                    let actualResult = mkCList input, countUniques input
                    Expect.equal actualResult expectedResult "Failed to produce a correct list \
                        on a source node with two children (all values differ)"

            testCase "A source node with two children and a child's child (all values match)"
                <| fun _ ->
                    let input = Node("abba", Cons(Node("abba", Cons(Leaf("abba"), Empty)), Cons(Leaf("abba"), Empty)))
                    let expectedResult = Cons("abba", Cons("abba", Cons("abba", Cons("abba", Empty)))), 1
                    let actualResult = mkCList input, countUniques input
                    Expect.equal actualResult expectedResult "Failed to produce a correct list \
                        on a source node with two children and a child's child (all values match)"

            testCase "A single source node that contains an empty list as a value"
                <| fun _ ->
                    let input1 = Node(Empty, Empty)
                    let input2 = Leaf(Empty)
                    let expectedResult = Cons(Empty, Empty), 1
                    let actualResult1 = mkCList input1, countUniques input1
                    let actualResult2 = mkCList input2, countUniques input2
                    Expect.equal (actualResult1 = expectedResult && actualResult2 = expectedResult) true "Failed to produce \
                        a correct list that has an empty list as an element."

            testProperty "Number of unique elements in a tree must be no more than number of all elements."
               <| fun tree ->
                    let lst = mkCList tree, countUniques tree
                    let length = getLength (fst lst)
                    snd lst <= length

            testProperty "A constructed list cannot be empty"
                <| fun tree ->
                    let result = mkCList tree, countUniques tree
                    getLength (fst result) > 0 && snd result >= 0

            testProperty "Constructing a list from a given tree then making a set of values \
                          should produce the same output as making a set of values from a tree straight away."
                <| fun tree ->

                    // Defines a folder function for CList.fold
                    let lstUniques lst =
                        let hSetAdd (hSet: HashSet<'Value>) item =
                            hSet.Add item |> ignore
                            hSet

                        let hSet = HashSet<'Value>()
                        CLists.fold hSetAdd hSet lst

                    // Defines a folder function for NTree.fold
                    let mkHashSet tree =
                        let hSetAdd (hSet: HashSet<'Value>) value =
                            hSet.Add value |> ignore
                            hSet

                        let hSet = HashSet<'Value>()
                        fold hSetAdd hSet tree

                    // We fold a given tree into a set of its elements (set1) and a CList.
                    // After which we fold a CList into the set of its elements (set2).
                    // Both sets (set1 and set2) should be subsets of each other.
                    let resultFromTree = mkCList tree, mkHashSet tree
                    let setFromTree = snd resultFromTree
                    let setFromCList = lstUniques <| fst resultFromTree

                    Expect.equal (setFromTree.IsSubsetOf setFromCList && setFromCList.IsSubsetOf setFromTree) true "Did not produce equal sets."
]

