namespace HomeWork3

open CLists
open Microsoft.FSharp.Core

module NTrees =

    /// This type represents an N-ary tree.
    // Each node in such tree may have any number of children from 0 to n.
    // Every node consists of its value and a list of the node's children.
    type NTree<'value> =
        | Leaf of value: 'value
        | Node of parent: 'value * children: CList<NTree<'value>>

    let example =
        Node(
            1,
            Cons(
                Node(
                    2,
                    Cons(
                        Node(3, Cons(Node(4, Cons(Leaf(5), Cons(Leaf(6), Cons(Leaf(7), Cons(Leaf(8), Empty))))), Empty)),
                        Empty
                    )
                ),
                Cons(Leaf(1), Empty)
            )
        )
    //                            Node(1)
    //                          /         \
    //                      Node(2)      Leaf(1)
    //                       /               \
    //                   Node(3)            Empty
    //                 | | | | |
    //     Node(4) Leaf(5) Leaf(6) Leaf(7) Leaf(8)
    //      /       |        |       |        |
    // Leaf(15)   Empty    Empty   Empty    Empty



    // HomeWork 3 - Task 2.
    /// This function traverses an NTree and collects values in its nodes
    /// while constructing a CList in a process.
    /// Returns the tuple of constructed CList and the set of its elements.
    let makeCList (tree: NTree<'value>) : CList<'value> * int =

        match tree with
        // If a given tree has only a single Leaf, return the result.
        // Otherwise traverse the tree.
        | Leaf value -> Cons(value, Empty), 1
        | Node (value, children) ->

            // Concat function is not present in this branch
            // so this sub-function will be removed once i merge this branch with HomeWork2.
            let rec concat (lst1: CList<'value>) (lst2: CList<'value>) : CList<'value> =
                match lst1 with
                | Empty -> lst2
                | Cons (head, tail) -> Cons(head, concat tail lst2)

            /// This sub-function visits nodes and children of an N-arty tree.
            // We traverse the tree from top to bottom, from left to right.
            // In essence there are two recursive functions that run "in parallel".
            // On one hand we recursively only visit nodes and ignore the children,
            // while on the other hand we ignore the node and go for the children.
            // On each call the function returns a tuple.
            // The first element of the tuple is a tuple of linked lists of all nodes
            // and children that were visited.
            // The second element is a set of values in visited nodes.
            // On each call we go one node deeper and one child further.
            let rec visit (family: CList<NTree<'value>>) : (CList<'value> * CList<'value>) * Set<'value> =
                match family with

                // Current link has no new nodes and node before has no more children.
                // Sequence ends.
                | Empty -> (Empty, Empty), Set.empty

                // Current link has a single Leaf with no children.
                // This link is the last node and the last child of
                // the parent node at the same time.
                // Add the value to the list of nodes and the list of children.
                | Cons (Leaf value, Empty) -> (Cons(value, Empty), Cons(value, Empty)), Set.empty.Add(value)

                // Current link has a single Leaf and
                // some other children of the parent node.
                // Add the value to the list of nodes and continue
                // visiting children of the parent node.
                | Cons (Leaf value, children) ->
                    let newChildren = visit children

                    (Cons(value, fst <| fst newChildren), snd <| fst newChildren),
                    Set.empty.Add(value) + snd newChildren

                // Current link has a node with its children
                // and further on the list are the children of the parent node.
                // Add the value of the current node to the list of nodes
                // and start visiting its children.
                // Also continue visiting the children of the parent node.
                | Cons (Node (value, newChildren), children) ->
                    let newestChildren = visit newChildren
                    let parentChildren = visit children

                    (Cons(value, fst <| fst newestChildren), snd <| fst parentChildren),
                    Set.empty.Add(value)
                    + snd newestChildren
                    + snd parentChildren

            // Visit source node's children and start traversing the tree.
            // Then concatenate the list of collected nodes and the list of collected children.
            let harvest (children: CList<NTree<'value>>) : CList<'value> * int =
                let familyTree = visit children
                let nodeList = fst <| fst familyTree
                let childrenList = snd <| fst familyTree
                let set = snd familyTree
                Cons(value, concat nodeList childrenList), set.Count


            // Return the list and the set.
            harvest children
