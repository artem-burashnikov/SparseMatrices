namespace Homework2

open CLists
open OOPLists

module AlgList =

    // Homework 2 - Task 3 - Concat.
    /// Function concatenates two linked lists.
    // Traverse the first list until Empty.
    // Place the second list at Empty.
    let rec concat (lst1: CList<'Value>) (lst2: CList<'Value>) : CList<'Value> =
        match lst1 with
        | Cons (head, tail) -> Cons(head, concat tail lst2)
        | Empty -> lst2



    // Homework 2 - Task 1 - Bubble sort.
    /// Function sorts a linked list using a bubble sorting algorithm.
    // First we calculate the number n of elements in a given list.
    // Then we recursively call the sorting function n times and
    // on each call some element ends up moving to its place .
    let bubbleSort (lst: CList<'Value>) : CList<'Value> =

        // Compare two consecutive values and swap them if required.
        // On each complete cycle this function puts a single element
        // on its spot in a sorted list.
        let rec sort (lst: CList<'Value>) : CList<'Value> =
            match lst with
            | Empty -> Empty
            // A single element is already sorted.
            | Cons (head, Empty) -> Cons(head, Empty)
            // Swap two consecutive values accordingly.
            | Cons (head1, Cons (head2, tail)) ->
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
        looper lst <| CLists.getLength lst



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
        | Cons (head, Empty) -> Cons(head, Empty)
        | _ ->

            // This sub-function takes a linked list and returns a tuple.
            // First part of the tuple (fst) consists of all elements of a given list,
            // that are less than or equal to the pivot.
            // Second part of the tuple (snd)
            // consists of all elements that greater than the pivot.
            let rec partition (lst: CList<'Value>) (pivot: 'Value) : CList<'Value> * CList<'Value> =
                match lst with
                | Empty -> Empty, Empty
                | Cons (head, tail) ->
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
                | Cons (head, Empty) -> Cons(head, Empty)
                | Cons (head, tail) ->
                    let parts = partition tail head
                    concat (sort <| fst parts) (Cons(head, sort <| snd parts))

            // Return a sorted list.
            sort lst



module OOPList =

    // Homework 2 - Task 3 - Concat.
    /// Function concatenates two linked lists of a type IList.
    // Traverse an IList object until the Empty type is reached.
    // Place the second IList at this point.
    // Return a joined IList.
    let rec concat (lst1: IList<'Value>) (lst2: IList<'Value>) : IList<'Value> =
        match lst1 with
        | :? List<'Value> as lst -> List(lst.Head, concat lst.Tail lst2)
        | :? EmptyList<'Value> -> lst2
        | _ ->
            failwith
                $"Task2.HomeWork2.OOPList.concat: \
                        IList type was expected in matching, but given \
                        lst1: %A{lst1.GetType()}, lst2: %A{lst2.GetType()}"



    // Homework 2 - Task 1 - Bubble sort.
    /// Function sorts a linked list using a bubble sorting algorithm.
    let bubbleSort (lst: IList<'Value>) : IList<'Value> =

        // This is the main sorting function.
        // It looks at every value in a linked list cell, compares adjacent cell's values
        // and swaps them if necessary.
        // After a complete cycle a single element takes its place in a sorted list.
        let rec sort (lst: IList<'Value>) =
            match lst with
            | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
            | :? List<'Value> as lst ->
                if lst.Tail :? EmptyList<'Value> then
                    lst
                else

                    let head1 = lst.Head
                    let head2 = headSeparator lst.Tail
                    let tail2 = tailSeparator lst.Tail

                    if head1 >= head2 then
                        List(head2, sort (List(head1, tail2)))
                    else
                        List(head1, sort lst.Tail)
            | _ ->
                failwith
                    $"Task2.HomeWork2.OOPList.bubbleSort.sort: \
                                IList type was expected in matching, but given %A{lst.GetType()}"

        // This sub-function calls the sorting function n times.
        // This way all elements end up on their places in a final sorted linked list.
        let rec looper lst counter =
            match counter with
            | 0 -> lst
            | counter -> looper (sort lst) (counter - 1)

        // Sort the list.
        looper lst <| getLength lst



    // Homework 2 - Task 2 - Quicksort.
    /// Sort a linked list using a quicksort algorithm.
    // The first element in a list is chosen as a pivot.
    // First we partition a list into two.
    // The first partition has all elements that are smaller or equal than the pivot.
    // The second partition has all elements that are bigger than the pivot.
    // We then concatenate all three parts in an appropriate order
    // while recursively sorting the partitions the same way.
    let qSort (lst: IList<'Value>) : IList<'Value> =
        match lst with
        // No need to sort an empty list or a list that has a single element.
        // Return immediately.
        | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
        | :? List<'Value> as lst ->
            if lst.Tail :? EmptyList<'Value> then
                lst
            else

                // This sub-function takes a linked list and returns a tuple.
                // First part of the tuple (fst) consists of all elements of a given list,
                // that are less than or equal to the pivot.
                // Second part of the tuple (snd)
                // consists of all elements that are greater than the pivot.
                let rec partition (lst: IList<'Value>) (pivot: 'Value) =
                    match lst with
                    | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>, EmptyList() :> IList<'Value>
                    | :? List<'Value> as lst ->
                        let parts = partition lst.Tail pivot

                        if lst.Head <= pivot then
                            List(lst.Head, fst parts), snd parts
                        else
                            fst parts, List(lst.Head, snd parts)
                    | _ ->
                        failwith
                            "Task2.HomeWork2.OOPList.qSort.partition \
                                     caused an exception in matching"

                // This is the main sorting sub-function.
                // It sorts two partitions and concatenates them with the pivot.
                let rec sort (lst: IList<'Value>) : IList<'Value> =
                    match lst with
                    // An empty list or a list that has a single element is already sorted.
                    // Otherwise we concatenate while making recursive calls on partitions.
                    | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
                    | :? List<'Value> as lst ->
                        if lst.Tail :? EmptyList<'Value> then
                            lst
                        else
                            let parts = partition lst.Tail lst.Head
                            concat (sort <| fst parts) (List(lst.Head, sort <| snd parts))
                    | _ -> failwith "Task2.HomeWork2.OOPList.qSort.sort caused an exception in matching"

                // Return a sorted list.
                sort lst

        // Unhandled exception
        | _ -> failwith "Task2.HomeWork2.qSort caused an exception in matching."
