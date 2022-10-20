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



    // HomeWork 3.
    /// This function traverses an NTree and collects values in its nodes
    /// while constructing a CList in a process.
    /// Returns the tuple of constructed CList and the count of the tree's unique elements.
    let makeCList (tree: NTree<'value>) =
        match tree with
        // If a given tree has only a single Leaf, return the result.
        // Otherwise traverse the tree.
        | Leaf value -> Cons(value, Empty), 1
        | Node (value, children) ->

            // Initialize a HashSet of values.
            // Each value from a visited node will be added to this HashSet.
            let hSet = new HashSet<'value>()

            /// This sub-function visits nodes and children of an N-ary tree.
            let rec visit family =
                match family with
                // If the node is empty, return nothing.
                | Empty -> Empty
                // If a child is a Leaf, then grab its value
                // and continue visiting children of the parent node.
                | Cons (Leaf value, children) ->
                    hSet.Add(value) |> ignore
                    Cons(value, visit children)
                // If a child is a Node, then grab its value, visit its children,
                // visit parent node's children and concatenate both sub-lists.
                | Cons (Node (value, depthChildren), widthChildren) ->
                    hSet.Add(value) |> ignore

                    concat
                    <| Cons(value, visit depthChildren)
                    <| visit widthChildren

            // Add value at the source to the head of the list.
            // Start traversing the tree by visiting the source node's children.
            let construct parent children = Cons(parent, visit children)

            // Return the required result which is a constructed CList
            // with a counter of unique values found in a given tree.
            let result =
                hSet.Add(value) |> ignore
                construct value children, hSet.Count

            result
