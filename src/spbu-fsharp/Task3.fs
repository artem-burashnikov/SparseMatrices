namespace HomeWork3

open System.Collections.Generic
open CLists
open Microsoft.FSharp.Core

module NTrees =

    /// This type represents an N-ary tree.
    // Each node in such tree may have any number of children.
    // Every node consists of its value and a list of the node's children which may be empty.
    type NTree<'Value> =
        | Leaf of value: 'Value
        | Node of parent: 'Value * children: CList<NTree<'Value>>



    /// General n-ary tree folding function.
    let rec fold folder acc item =
        let recurse = fold folder

        match item with
        | Leaf value -> folder acc value
        | Node (value, children) -> CLists.fold recurse (folder acc value) children



    /// Function makes a set of values in the nodes of an n-ary tree.
    // We pass hSetAdd as folder function to the general tree folding function.
    // Returns a set of elements.
    let setFromValues tree =
        let hSetAdd (hSet: HashSet<'Value>) value =
            hSet.Add value |> ignore
            hSet

        let hSet = HashSet<'Value>()
        fold hSetAdd hSet tree



    /// Function constructs a linked list of type CList from the values in the nodes of n-ary tree.
    // lstAdd is passed as folder function to the general tree folding function.
    // Returns a CList that has all values.
    let cListConstruct tree =
        // The defined order of arguments (1st: elem; 2nd: cList) is important.
        // When this gets passed as a folder function, the accumulator will be the first argument.
        let lstAdd cList elem = Cons(elem, cList)
        fold lstAdd Empty tree


    // HomeWork 3 result.
    let traverse tree =
        cListConstruct tree, (setFromValues tree).Count
