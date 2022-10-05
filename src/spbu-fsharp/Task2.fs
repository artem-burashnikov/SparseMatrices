
module HomeWork2

open Lists



// Homework 2 - Task 3 - Concat (MyList)
/// Function concatenates two linked lists.
// Traverse the first list until Empty.
// Place the second list at Empty.
let rec concat (lst1: MyList<'value>) (lst2: MyList<'value>) : MyList<'value> =
    match lst1 with
    | Cons (head, tail) -> Cons(head, concat tail lst2)
    | Empty -> lst2



// Homework 2 - Task 1 - Bubble sort (MyList).
/// Function sorts a linked list using a bubble sorting algorithm.
// First we calculate the number n of elements in a given list.
// Then we recursively call the sorting function n times and
// on each call some element ends up moving to its place .
let bubbleSort (lst: MyList<'value>) : MyList<'value> =

    // Compare two consecutive values and swap them if required.
    // On each complete cycle this function puts a single element
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
                    (concat
                         (Cons(head, Empty))
                         (sort (biggerElements tail head)))

        // Return a sorted list.
        sort lst



// Homework 2 - Task 3 - Concat (MyOOPList)
/// Function concatenates two linked lists of a type IList.
// Traverse an IList object until the Empty type is reached.
// Place the second IList at this point.
// Return a joined IList.
// lst1 is the list that is being traversed.
// lst2 is attached at the end of lst1.
let rec concatOOP (lst1: IList<'value>) (lst2: IList<'value>) : IList<'value> =
    match lst1 with
    | :? MyOOPNonEmptyList<'value> as lst ->
        MyOOPNonEmptyList(lst.Head, concatOOP lst.Tail lst2)
    | :? MyOOPEmptyList<'value> -> lst2
    | _ -> failwith "Task2.OOPConcat: Was given some object other than IList"



// Homework 2 - Task 1 - Bubble sort (MyOOPList)
/// Function sorts a linked list using a bubble sorting algorithm.
let bubbleSortOOP (lst: IList<'value>) : IList<'value> =

    // This is a main sorting function.
    // It looks at every value in a linked list cell, compares adjacent cell's values
    // and swaps them if necessary.
    // After a complete cycle a single element takes its place in a sorted list.
    let rec sort (lst: IList<'value>) =
        match lst with
        | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
        | :? MyOOPNonEmptyList<'value> as lst ->
            if lst.Tail :? MyOOPEmptyList<'value> then
                lst
            else

                let head1 = lst.Head
                let head2 = headSeparator lst.Tail
                let tail2 = tailSeparator lst.Tail

                if head1 >= head2 then
                    MyOOPNonEmptyList(head2, sort (MyOOPNonEmptyList(head1, tail2)))
                else
                    MyOOPNonEmptyList(head1, sort lst.Tail)
        | _ -> failwith "Task2.bubbleSortOOP.sort: IList type was expected."

    // This sub-function calls the sorting function n times.
    // This way all elements end up on their places in a final sorted linked list.
    let rec looper lst counter =
        match counter with
        | 0 -> lst
        | counter -> looper (sort lst) (counter - 1)

    // Sort the list.
    looper lst (getLengthOOP lst)



// Homework 2 - Task 2 - Quicksort (MyList)
/// Sort a linked list using a quicksort algorithm.
// The first element in a list is chosen as a pivot.
// First we partition a list into two.
// The first partition has all elements that are smaller or equal than the pivot.
// The second partition has all elements that are bigger than the pivot.
// We then concatenate all three parts in an appropriate order
// while recursively sorting the partitions the same way.
let qSortOOP (lst: IList<'value>) : IList<'value> =
    match lst with
    // No need to sort an empty list or a list that has a single element.
    // Return immediately.
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            lst
        else

        // This sub-function returns all elements
        // that are less than or equal to the pivot.
        let rec smallerElements (lst: IList<'value>) (pivot: 'value) : IList<'value> =
            match lst with
            // If an element is less than or equal to the pivot, then accumulate it.
            // Otherwise ignore.
            | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
            | :? MyOOPNonEmptyList<'value> as lst ->
                if lst.Tail :? MyOOPEmptyList<'value> then

                    if lst.Head <= pivot then
                        MyOOPNonEmptyList(lst.Head, lst.Tail)
                    else
                        MyOOPEmptyList() :> IList<'value>

                else

                    if lst.Head <= pivot then
                        MyOOPNonEmptyList(lst.Head, smallerElements lst.Tail pivot)
                    else
                        smallerElements lst.Tail pivot

            | _ -> failwith "Task2.qSortOOP: smallerElements caused an exception in matching."


        // This function returns all elements that
        // are greater than the pivot.
        let rec biggerElements (lst: IList<'value>) (pivot: 'value) : IList<'value> =
            match lst with
            // If an element is greater than the pivot, then accumulate it.
            // Otherwise ignore.
            | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
            | :? MyOOPNonEmptyList<'value> as lst ->
                if lst.Tail :? MyOOPEmptyList<'value> then

                    if lst.Head > pivot then
                        MyOOPNonEmptyList(lst.Head, lst.Tail)
                    else
                        MyOOPEmptyList() :> IList<'value>

                else

                    if lst.Head > pivot then
                        MyOOPNonEmptyList(lst.Head, biggerElements lst.Tail pivot)
                    else
                        biggerElements lst.Tail pivot
            | _ -> failwith "Task2.qSortOOP: biggerElements caused an exception in matching."


        // This is the main sorting function.
        // It sorts two partitions and concatenates them with the pivot.
        let rec sort (lst: IList<'value>) : IList<'value> =
            match lst with
            // An empty list or a list that has a single element is already sorted.
            // Otherwise we concatenate and make a recursive call on partitions.
            | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
            | :? MyOOPNonEmptyList<'value> as lst ->
                if lst.Tail :? MyOOPEmptyList<'value> then
                    lst

                else
                    concatOOP
                        (sort (smallerElements lst.Tail lst.Head))
                        (concatOOP
                             (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))
                             (sort (biggerElements lst.Tail lst.Head)))
            | _ -> failwith "Task2.qSortOOP.sort caused an exception in matching"

        // Return a sorted list.
        sort lst

    | _ -> failwith "Task2.qSortOOP caused an exception in matching."
