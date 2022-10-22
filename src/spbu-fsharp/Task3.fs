namespace HomeWork3

open System.Collections.Generic
open CLists
open Homework2.AlgList
open Microsoft.FSharp.Core

module NTrees =

    /// This type represents an N-ary tree.
    // Each node in such tree may have any number of children.
    // Every node consists of its value and a list of the node's children which may be empty.
    type NTree<'value> =
        | Leaf of value: 'value
        | Node of parent: 'value * children: CList<NTree<'value>>

    let traverse tree =
        /// General n-ary tree folding function.
        // It requires two operations to be defined:
        // fAdd - which would add item in each visited node to the accumulator.
        // fMerge - which would combine resulting accumulators from different sub-trees.
        let rec fold fAdd fMerge acc item =
            let recurse = fold fAdd fMerge

            match item with
            | Leaf value -> fAdd acc value
            | Node (value, children) ->
                match children with
                | Empty -> fAdd acc value
                // I have some idea why the following line works.
                // I pass "recurse" as a function to CLists.fold.
                // But what is "recurse" exactly then?
                // I need a function that would work as a folder on a list of trees.
                // Which is what recurse just happened to be.
                // But why is "recurse" a valid function to be passed?
                | Cons (node, children) -> CLists.fold recurse (recurse <| fAdd acc value <| node) children

        /// Function counts unique values in the nodes of an n-ary tree.
        // We pass hSetAdd and hSetUnite as folder functions to the general tree folding function.
        // Returns an integer - number of unique elements.
        let uniqueValues tree =
            let hSetAdd (hSet: HashSet<'value>) value =
                hSet.Add value |> ignore
                hSet

            let hSetUnite (hSet1: HashSet<'value>) (hSet2: HashSet<'value>) =
                hSet1.UnionWith hSet2
                hSet1

            let hSet = HashSet<'value>()
            let result = fold hSetAdd hSetUnite hSet tree
            result.Count

        /// Function constructs a linked list of type CList from the values in the nodes of n-ary tree.
        // lstAdd and lstUnite are passed as folder functions to the general tree folding function.
        // Returns a CList that has all values.
        let cListConstruct tree =
            // The defined order of arguments (1st: elem; 2nd: cList) is important.
            // When this gets passed as a folder function, the accumulator will be the first argument.
            let lstAdd cList elem = Cons(elem, cList)
            let lstUnite lst1 lst2 = concat lst1 lst2
            fold lstAdd lstUnite Empty tree

        // Final result.
        cListConstruct tree, uniqueValues tree
