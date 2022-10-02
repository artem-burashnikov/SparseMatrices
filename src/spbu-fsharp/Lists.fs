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

// let fromMyListToMyOOPList lst =
// This function takes two linked lists
// and checks if their corresponding nodes are equal.
// Returns true if lists are identical.
// False otherwise.
let rec checkEqual lst1 lst2 : bool =
    match lst1, lst2 with
    | Empty, Empty -> true
    | Cons (head1, tail1), Cons (head2, tail2) ->
        if head1 = head2 then
            checkEqual tail1 tail2
        else
            true
    | _ -> false



/// Count the number of elements in a list.
let rec getLength (lst: MyList<'value>) : int =
    match lst with
    | Empty -> 0
    | Cons (_, tail) -> 1 + getLength tail
