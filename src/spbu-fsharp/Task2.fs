
module HomeWork2

open Lists

/// Count the number of elements in a list.
let rec getLength (lst: MyList<'value>) : int =
    match lst with
    | Empty -> 0
    | Cons (_, tail) -> 1 + getLength tail

// Homework 2 - Task 3 - Concat (MyList)
/// Concatenates two lists.
// Traverse the first list until Empty.
// Place the second list at Empty.
let rec concat (lst1: MyList<'value>) (lst2: MyList<'value>) : MyList<'value> =
    match lst1 with
    | Cons (head, tail) -> Cons(head, concat tail lst2)
    | Empty -> lst2



// Homework 2 - Task 1 - Bubble sort (MyList).
/// Sort a linked list using a bubble sorting algorithm.
// First we calculate the number n of elements in a given list.
// Then we recursively call the sorting function n times and
// on each call some element ends up moving to its place .
let bubbleSort (lst: MyList<'value>) : MyList<'value> =

    // Compare two consecutive values and swap them if required.
    // On each call this function puts a single element
    // on its spot in a sorted list.
    let rec sort (lst: MyList<'value>) : MyList<'value> =
        match lst with
        | Empty -> Empty
        // A single element is already sorted.
        | Cons (head, Empty) -> Cons(head, Empty)
        // Swap two consecutive values accordingly.
        | Cons (head1, Cons (head2, tail)) ->
            if head1 >= head2 then
                Cons(head2, sort (Cons(head1, tail)))
            else
                Cons(head1, sort (Cons(head2, tail)))

    // Call a sorting function n times,
    // where n = counter is the length of a given linked list.
    // This way all elements end up on their spots in the final sorted list.
    let rec looper lst counter =
        match counter with
        | 0 -> lst
        | counter -> looper (sort lst) (counter - 1)

    // Return a sorted list.
    looper lst (getLength lst)



// Homework 2 - Task 2 - Quicksort (MyList)
/// Sort a linked list using a quicksort algorithm.
// The first element in a list is chosen as a pivot.
// First we partition a list into two.
// The first partition has all elements that are smaller or equal than the pivot.
// The second partition has all elements that are bigger than the pivot.
// We then concatenate all three parts in an appropriate order
// while recursively sorting the partitions the same way.
let qSort (lst: MyList<'value>) : MyList<'value> =
    match lst with
    // No need to sort an empty list or a list that has a single element.
    // Return immediately.
    | Empty -> Empty
    | Cons (head, Empty) -> Cons(head, Empty)
    | _ ->

        // This sub-function returns all elements
        // that are less than or equal to the pivot.
        let rec smallerElements (lst: MyList<'value>) (pivot: 'value) : MyList<'value> =
            match lst with
            // If an element is less than or equal to the pivot, then accumulate it.
            // Otherwise ignore.
            | Empty -> Empty
            | Cons (head, Empty) ->
                if head <= pivot then
                    Cons(head, Empty)
                else
                    Empty
            | Cons (head, tail) ->
                if head <= pivot then
                    Cons(head, smallerElements tail pivot)
                else
                    smallerElements tail pivot

        // This function returns all elements that
        // are greater than the pivot.
        let rec biggerElements (lst: MyList<'value>) (pivot: 'value) : MyList<'value> =
            match lst with
            // If an element is greater than the pivot, then accumulate it.
            // Otherwise ignore.
            | Empty -> Empty
            | Cons (head, Empty) ->
                if head > pivot then
                    Cons(head, Empty)
                else
                    Empty
            | Cons (head, tail) ->
                if head > pivot then
                    Cons(head, biggerElements tail pivot)
                else
                    biggerElements tail pivot

        // This is the main sorting function.
        // It sorts two partitions and concatenates them with the pivot.
        let rec sort (lst: MyList<'value>) : MyList<'value> =
            match lst with
            // An empty list or a list that has a single element is already sorted.
            // Otherwise we concatenate and make a recursive call on partitions.
            | Empty -> Empty
            | Cons (head, Empty) -> Cons(head, Empty)
            | Cons (head, tail) ->
                concat
                    (sort (smallerElements tail head))
                    (concat (Cons(head, Empty)) (sort (biggerElements tail head)))

        // Return a sorted list.
        sort lst
