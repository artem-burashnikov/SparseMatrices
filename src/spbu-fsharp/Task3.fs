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
        let rec fold fAdd fMerge acc item =
            let recurse = fold fAdd fMerge

            match item with
            | Leaf value -> fAdd acc value
            | Node (value, children) ->
                match children with
                | Empty -> fAdd acc value
                | Cons (node, Empty) -> recurse <| fAdd acc value <| node
                // I have a very little idea why the following line works.
                // I pass "recurse" as a function to CLists.fold.
                // But what is "recurse" exactly then?
                // The purpose was very straightforward - a function that would fold a list of trees was needed.
                // Which recurse just happened to be.
                // But why is "recurse" a valid function to be passed?
                | Cons (node, children) -> CLists.fold recurse (recurse <| fAdd acc value <| node) children

        let uniqueValues tree =
            let hSetAdd (hSet: HashSet<'value>) value =
                hSet.Add value |> ignore
                hSet

            let hSetUnite (hSet1: HashSet<'value>) (hSet2: HashSet<'value>) =
                hSet1.UnionWith hSet2
                hSet1

            let hSet = HashSet<'value>()
            fold hSetAdd hSetUnite hSet tree

        let cListConstruct tree =
            let lstAdd cList elem = Cons(elem, cList)
            let lstUnite lst lst1 = concat lst lst1
            fold lstAdd lstUnite Empty tree

        cListConstruct tree, (uniqueValues tree).Count
