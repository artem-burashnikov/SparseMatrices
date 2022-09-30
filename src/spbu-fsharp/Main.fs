namespace spbu_fsharp

open Expecto
open FsCheck
open Microsoft.FSharp.Core


type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty



type IList<'value> =
    interface
    end

type MyOOPNonEmptyList<'value>(head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

type MyOOPEmptyList<'value>() =
    interface IList<'value>

type IActor<'inType, 'outType> =
    abstract Do: 'inType -> 'outType



module Main =

    module public HomeWork1 =

        // Homework 1 - Task 1.
        // Power function.
        // This function takes two numbers (base and exponent)
        // and iteratively calculates a power of base to the exponent.
        let pow (arg: int) (exp: int) : int =

            // This operation is not defined
            if arg = 0 && exp = 0 then
                failwith "Undefined"

            // Accumulate the product through a loop.
            let mutable result = 1

            for i = 1 to abs exp do
                result <- result * arg

            // For a positive exponent return the value as is.
            // For a negative exponent inverse the number.
            if exp > 0 then result else 1 / result



        // Homework 1 - Task 2.
        // Quick power function.
        // Ths function takes two numbers (base and exponent)
        // and recursively calculates a power of base to the exponent.
        let rec qPow (arg: int) (exp: int) : int =

            // This operation is not defined.
            if arg = 0 && exp = 0 then
                failwith "undefined"

            let result =

                // Recursion base case.
                if exp = 0 then
                    1
                elif exp = 1 then
                    arg
                else
                    // Divide the exponent by half (floor is taken for an odd argument).
                    let halve = qPow arg (abs exp / 2)
                    // To get an even exponent multiply its halves.
                    if exp % 2 = 0 then
                        halve * halve
                    else
                        // For an odd exponent additionally multiply by the arg.
                        halve * halve * arg

            // For a positive exponent return the value as is.
            // For a negative exponent inverse the number.
            if exp > 0 then result else 1 / result



        // Homework 1 - Task 3.
        // Difference between the min and the max value in an array.
        // The function takes an array of real numbers and returns the difference
        // between the maximum and the minimum values.
        let diff (arr: float array) : float =

            let result = // This will be returned.

                if arr = [||] then
                    failwith "undefined"
                else

                    let mutable mx = arr[0]
                    let mutable mn = arr[0]

                    // Look at each element in an array from left to right
                    // and iteratively find min and max values.
                    for i = 1 to arr.Length - 1 do

                        // Max on the current iteration.
                        if arr[i] > mx then mx <- arr[i]

                        // Min on the current iteration.
                        if arr[i] < mn then mn <- arr[i]

                    mx - mn // The desired value

            result



        // Homework 1 - Task 4.
        // This function returns the array of all odd integers
        // strictly in between two given integers.
        let allOdds (num1: int) (num2: int) : int array =

            // Determine the range
            let smallerNum: int = if num1 <= num2 then num1 else num2

            let biggerNum: int = if num1 <= num2 then num2 else num1

            // Make an array of all odd integers in the specified range
            let result: int array =
                [| for i in (smallerNum + 1) .. (biggerNum - 1) do
                       if abs i % 2 = 1 then yield i |]

            result // Return the array.


    module public HomeWork2 =


        // Homework 2 - Task 3 - Concat (MyList)
        // This function concatenates two lists.
        // Traverse the first list until Empty.
        // Place the second list at Empty.
        let rec concat (lst1: MyList<'value>) (lst2: MyList<'value>) : MyList<'value> =
            match lst1 with
            | Cons (head, tail) -> Cons(head, concat tail lst2)
            | Empty -> lst2

        let rec length (lst: MyList<'value>) : int =
            match lst with
            | Empty -> 0
            | Cons (_, tail) -> 1 + length tail

        // Homework 2 - Task 1 - Bubble sort (MyList).
        // This function sorts a linked list.
        // First we calculate the number (n) of elements in a given list.
        // Then we recursively call the sorting function n times and
        // on each call some element ends up moving to its
        // place .
        let bubbleSort (lst: MyList<'value>) : MyList<'value> =

            // This function counts the number of elements.
            let rec getLength (lst: MyList<'value>) : int =
                match lst with
                | Empty -> 0
                | Cons (_, tail) -> 1 + getLength tail

            // Compare two consecutive elements and swap them if required.
            // On each call this function puts a single element
            // on its spot in a sorted list.
            let rec sort (lst: MyList<'value>) : MyList<'value> =
                match lst with
                | Empty -> Empty
                // A single element is already sorted.
                | Cons (head, Empty) -> Cons(head, Empty)
                // Swap two consecutive elements accordingly.
                | Cons (head1, Cons (head2, tail)) ->
                    if head1 >= head2 then
                        Cons(head2, sort (Cons(head1, tail)))
                    else
                        Cons(head1, sort (Cons(head2, tail)))

            // This function calls a sorting function n times,
            // where n is the length of a given linked list.
            // This way all elements end up on their spots in a sorted list.
            let rec looper (lst: MyList<'value>) (counter: int) : MyList<'value> =
                match counter with
                | 0 -> lst
                | counter -> looper (sort lst) (counter - 1)

            // Returns a sorted list.
            looper lst (getLength lst)



        // Homework 2 - Task 2 - Quicksort (MyList)
        // This function sorts a linked list.
        // The first element in a list is chosen as a pivot.
        // First we partition a list into two.
        // The first partition has all elements that are smaller or equal than the pivot.
        // The second partition has all elements that are bigger than the pivot.
        // We then concatenate all three in an appropriate order
        // while recursively sorting the partitions the same way.
        let qSort (lst: MyList<'value>) : MyList<'value> =
            match lst with
            // No need to sort an empty list or a list that has a single element.
            // Return immediately.
            | Empty -> Empty
            | Cons (head, Empty) -> Cons(head, Empty)
            | Cons (head, tail) ->

                // This function returns a new list
                // which elements are less than or equal to the pivot.
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

                // This function returns a new list
                // which elements are bigger than the pivot.
                let rec biggerElements (lst: MyList<'value>) (pivot: 'value) : MyList<'value> =
                    match lst with
                    // If an element is bigger than the pivot, then accumulate it.
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

                // This is a main sorting function.
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

                // Returns a sorted list.
                sort lst


    //
    // let OOPConcat (lst1: IList<'value>) (lst2: IList<'value>): IList<'value> =
    //     interface IList<'value>




    [<EntryPoint>]
    let main (argv: string array) =

        let myListGen = Arb.generate<MyList<int>>
        let samples = Gen.sample 100 20 myListGen

        // printfn $"res: {HomeWork2.qSort myListExample1}"
        0
