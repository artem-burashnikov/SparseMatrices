namespace spbu_fsharp

open Microsoft.FSharp.Core

module Main =

    // Custom List type
    type MyList<'value> =
        | Cons of head: 'value * tail: MyList<'value>
        | Empty

    // Custom map function for a MyList type.
    let rec map func lst =
        match lst with
        | Empty -> Empty
        | Cons (head, tail) -> Cons(func head, map func tail)



    // Homework 1 - Task 1.
    // Power function.
    // This function takes two numbers (base and exponent)
    // and iteratively calculates a power of base to the exponent.
    let pow (arg: float) (exp: int) : float =

        // This operation is not defined
        if arg = 0 && exp = 0 then
            failwith "Undefined"

        // Accumulate the product through a loop.
        let mutable result: float = 1.0

        for i = 1 to abs exp do
            result <- result * arg

        // For a positive exponent return the value as is.
        // For a negative exponent inverse the number.
        if exp > 0 then result else 1.0 / result



    // Homework 1 - Task 2.
    // Quick power function.
    // Ths function takes two numbers (base and exponent)
    // and recursively calculates a power of base to the exponent.
    let rec qPow (arg: float) (exp: int) : float =

        // This operation is not defined.
        if arg = 0 && exp = 0 then
            failwith "undefined"

        let result: float =

            // Recursion base case.
            if exp = 0 then
                1.0
            elif exp = 1 then
                arg
            else
                // Divide the exponent by half (floor is taken for an odd argument).
                let halve: float = qPow arg (abs exp / 2)
                // To get an even exponent multiply its halves.
                if exp % 2 = 0 then
                    halve * halve
                else
                    // For an odd exponent additionally multiply by the arg.
                    halve * halve * arg

        // For a positive exponent return the value as is.
        // For a negative exponent inverse the number.
        if exp > 0 then result else 1.0 / result



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



    // Homework 2 - Task 3.
    // Concatenate two lists of type MyList.
    let rec concat (lst1: MyList<'value>) (lst2: MyList<'value>) : MyList<'value> =
        match lst1 with
        | Cons (head, tail) -> Cons(head, concat tail lst2) // Traverse the list until Empty.
        | Empty -> lst2 // Place the second list at Empty.



    let bubbleSort (lst: MyList<'value>) : MyList<'value> =

        // Count number of elements in MyList.
        let rec getLength (lst: MyList<'value>) : int =
            match lst with
            | Empty -> 0
            | Cons (_, tail) -> 1 + getLength tail

        // Compare two consecutive elements in MyList and swap them if required.
        // After traversing the whole MyList from head to the end this function
        // places the biggest element at the end.
        let rec sort (lst: MyList<'value>) : MyList<'value> =
            match lst with
            | Empty -> Empty // Do nothing.
            | Cons (head, Empty) -> Cons(head, Empty) // A single element is already compared.
            // Swap two consecutive elements accordingly.
            | Cons (head1, Cons (head2, tail)) ->
                if head1 >= head2 then
                    Cons(head2, sort (Cons(head1, tail)))
                else
                    Cons(head1, sort (Cons(head2, tail)))

        // This function sorts MyList (counter = n) times, where n is the length of MyList.
        // This way all n elements end up on their spots in a sorted MyList.
        let rec looper (lst: MyList<'value>) (counter: int) : MyList<'value> =
            match counter with
            | 0 -> lst
            | counter -> looper (sort lst) (counter - 1)

        // At last returns a sorted MyList.
        looper lst (getLength lst)



    [<EntryPoint>]
    let main (argv: string array) =

        let myListExample1 =
            Cons(10, Cons(9, Cons(8, Cons(7, Cons(6, Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, Empty))))))))))

        let myListExample2 = Cons(5, Cons(5, Cons(5, Cons(1, Empty))))

        printfn $"res: {bubbleSort myListExample1}"
        0
