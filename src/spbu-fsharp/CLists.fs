module CLists

type CList<'Value> =
    | Cons of head: 'Value * tail: CList<'Value>
    | Empty

let rec map f lst =
    match lst with
    | Empty -> Empty
    | Cons(hd, tl) -> Cons(f hd, map f tl)

let rec fold folder acc lst =

    match lst with
    | Empty -> acc
    | Cons(head, tail) -> fold folder (folder acc head) tail

let rec concat (lst1: CList<'Value>) (lst2: CList<'Value>) : CList<'Value> =
    match lst1 with
    | Cons(head, tail) -> Cons(head, concat tail lst2)
    | Empty -> lst2

/// This function takes two linked lists
/// and checks if their corresponding nodes are equal.
// Returns true if lists are identical.
// False otherwise.
let rec checkEqual lst1 lst2 : bool =
    match lst1, lst2 with
    | Empty, Empty -> true
    | Cons(head1, tail1), Cons(head2, tail2) -> if head1 = head2 then checkEqual tail1 tail2 else true
    | _ -> false

/// Count the number of elements in a list.
let rec getLength (lst: CList<'Value>) : int =
    match lst with
    | Empty -> 0
    | Cons(_, tail) -> 1 + getLength tail


let bubbleSort (lst: CList<'Value>) : CList<'Value> =

    // Compare two consecutive values and swap them if required.
    // On each complete cycle this function puts a single element
    // on its spot in a sorted list.
    let rec sort (lst: CList<'Value>) : CList<'Value> =
        match lst with
        | Empty -> Empty
        // A single element is already sorted.
        | Cons(head, Empty) -> Cons(head, Empty)
        // Swap two consecutive values accordingly.
        | Cons(head1, Cons(head2, tail)) ->
            if head1 >= head2 then
                Cons(head2, sort <| Cons(head1, tail))
            else
                Cons(head1, sort <| Cons(head2, tail))

    // Call a sorting function n times,
    // where n = counter is the length of a given linked list.
    // This way all elements end up on their spots in the final sorted list.
    let rec looper lst counter =
        match counter with
        | 0 -> lst
        | _ -> looper (sort lst) (counter - 1)

    // Return a sorted list.
    looper lst <| getLength lst


// Homework 2 - Task 2 - Quicksort.
/// Sort a linked list using a quicksort algorithm.
// The first element in a list is chosen as a pivot.
// First we partition a list into two.
// The first partition has all elements that are smaller or equal than the pivot.
// The second partition has all elements that are bigger than the pivot.
// We then concatenate all three parts in an appropriate order
// while recursively sorting the partitions the same way.
let qSort (lst: CList<'Value>) : CList<'Value> =
    match lst with
    // No need to sort an empty list or a list that has a single element.
    // Return immediately.
    | Empty -> Empty
    | Cons(head, Empty) -> Cons(head, Empty)
    | _ ->

        // This sub-function takes a linked list and returns a tuple.
        // First part of the tuple (fst) consists of all elements of a given list,
        // that are less than or equal to the pivot.
        // Second part of the tuple (snd)
        // consists of all elements that greater than the pivot.
        let rec partition (lst: CList<'Value>) (pivot: 'Value) : CList<'Value> * CList<'Value> =
            match lst with
            | Empty -> Empty, Empty
            | Cons(head, tail) ->
                let parts = partition tail pivot

                if head <= pivot then
                    Cons(head, fst parts), snd parts
                else
                    fst parts, Cons(head, snd parts)

        // This is the main sorting sub-function.
        // It sorts two partitions and concatenates them with the pivot.
        let rec sort (lst: CList<'Value>) : CList<'Value> =
            match lst with
            // An empty list or a list that has a single element is already sorted.
            // Otherwise we concatenate and make a recursive call on partitions.
            | Empty -> Empty
            | Cons(head, Empty) -> Cons(head, Empty)
            | Cons(head, tail) ->
                let parts = partition tail head
                concat (sort <| fst parts) (Cons(head, sort <| snd parts))

        // Return a sorted list.
        sort lst
