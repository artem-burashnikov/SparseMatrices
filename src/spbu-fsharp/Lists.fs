module Lists


type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty

let rec map f lst =
    match lst with
    | Empty -> Empty
    | Cons (hd, tl) -> Cons (f hd, map f tl)

let go () =
    map ((+)1) (Cons (1,Cons(3,Empty)))

let _go () =
    map ((-)1) (Cons (1,Cons(3,Empty)))

type IList<'value> = interface end

type MyOOPNonEmptyList<'value> (head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

type MyOOPEmptyList<'value>() =
    interface IList<'value>

type IActor<'inType, 'outType> =
    abstract Do: 'inType -> 'outType

let rec oopMap (f:IActor<'value,'result>) (lst:IList<'value>) =
    if lst :? MyOOPEmptyList<'value>
    then MyOOPEmptyList() :> IList<'result>
    elif lst :? MyOOPNonEmptyList<'value>
    then
        let lst = lst :?> MyOOPNonEmptyList<'value>
        MyOOPNonEmptyList(f.Do lst.Head, oopMap f lst.Tail)
    else failwith "!!!"

let rec oopMap2 (f:IActor<'value,'result>) (lst:IList<'value>) =
    match lst with
    | :? MyOOPEmptyList<'value> ->
        MyOOPEmptyList () :> IList<'result>
    | :? MyOOPNonEmptyList<'value> as lst ->
        MyOOPNonEmptyList(f.Do lst.Head, oopMap f lst.Tail)
    | _ -> failwith "!!!"

type PlusOneActor () =
    interface IActor<int,int> with
        member this.Do x = x + 1

type MinusOneActor () =
    interface IActor<int,int> with
        member this.Do x = x - 1

let _go2() =
    let lst = MyOOPNonEmptyList(1,MyOOPNonEmptyList(3,MyOOPEmptyList()))
    oopMap (PlusOneActor()) lst

let go2() =
    let lst = MyOOPNonEmptyList(1,MyOOPNonEmptyList(3,MyOOPEmptyList()))
    oopMap (MinusOneActor()) lst



// This function takes two linked lists
// and checks if their corresponding nodes are equal.
// Returns true if lists are identical.
// False otherwise.
let rec checkEqual (myList1: MyList<'value>) (myList2: MyList<'value>) : bool =
    match myList1, myList2 with
    | Empty, Empty -> true
    | Cons (head1, tail1), Cons (head2, tail2) ->
        if head1 = head2 then
            checkEqual tail1 tail2
        else
            true
    | _ -> false



/// Count the number of elements in a list (MyList).
let rec getLength myList: int =
    match myList with
    | Empty -> 0
    | Cons (_, tail) -> 1 + getLength tail



/// Count the number of elements in a list (IList)
let rec getLengthOOP (myOOPList:IList<'value>) : int =
    match myOOPList with
    | :? MyOOPEmptyList<'value> -> 0
    | :? MyOOPNonEmptyList<'value> as myOOPList -> 1 + getLengthOOP myOOPList.Tail
    | _ -> failwith "Task2.getLengthOOP: Function only accepts IList types. \
                    Incorrect variable type was given."


/// Convert MyOOPList type to MyList type.
let rec toMyList (myOOPList: IList<'value>) =
    match myOOPList with
    | :? MyOOPEmptyList<'value> -> Empty
    | :? MyOOPNonEmptyList<'value> as myOOPList ->
        Cons(myOOPList.Head, toMyList myOOPList.Tail)
    | _ -> failwith "Error"


/// Convert MyList type to MyOOPList type.
let rec toMyOOPList myList =
    match myList with
    | Empty -> MyOOPEmptyList() :> IList<'value>
    | Cons(head, tail) -> MyOOPNonEmptyList(head, toMyOOPList tail)

/// Takes a linked list cell and return the its value.
let headSeparator (lst: IList<'value>) : 'value =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst -> lst.Head
    | _ -> failwith "Task2.headSeparator: Function only accepts MyOOPNonEmptyList type."

// Takes a linked list cell and return the next cell linked to it.
let tailSeparator (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst -> lst.Tail
    | _ -> failwith "Task2.tailSeparator: Function only accepts IList type."
