module OOPLists

type IList<'value> =
    interface
    end

type List<'value>(head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'value>() =
    interface IList<'value>

type IActor<'inType, 'outType> =
    abstract Do: 'inType -> 'outType

let rec oopMap (f: IActor<'value, 'result>) (lst: IList<'value>) =
    if lst :? EmptyList<'value> then
        EmptyList() :> IList<'result>
    elif lst :? List<'value> then
        let lst = lst :?> List<'value>
        List(f.Do lst.Head, oopMap f lst.Tail)
    else
        failwith "!!!"

let rec oopMap2 (f: IActor<'value, 'result>) (lst: IList<'value>) =
    match lst with
    | :? EmptyList<'value> -> EmptyList() :> IList<'result>
    | :? List<'value> as lst -> List(f.Do lst.Head, oopMap f lst.Tail)
    | _ -> failwith "!!!"

type PlusOneActor() =
    interface IActor<int, int> with
        member this.Do x = x + 1

type MinusOneActor() =
    interface IActor<int, int> with
        member this.Do x = x - 1

let _go2 () =
    let lst = List(1, List(3, EmptyList()))
    oopMap (PlusOneActor()) lst

let go2 () =
    let lst = List(1, List(3, EmptyList()))
    oopMap (MinusOneActor()) lst

/// Count the number of elements in a list (IList)
let rec getLength (lst: IList<'value>) : int =
    match lst with
    | :? EmptyList<'value> -> 0
    | :? List<'value> as lst -> 1 + getLength lst.Tail
    | _ ->
        failwith
            "OOPLists.getLength: Function only accepts IList types. \
                    Incorrect variable type was given."

/// Takes a linked list cell and returns its value.
let headSeparator (lst: IList<'value>) : 'value =
    match lst with
    | :? List<'value> as lst -> lst.Head
    | _ -> failwith "OOPLists.headSeparator: Function only accepts List type."

// Takes a linked list cell and returns the next cell linked to it.
let tailSeparator (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? EmptyList<'value> -> EmptyList() :> IList<'value>
    | :? List<'value> as lst -> lst.Tail
    | _ -> failwith "OOPLists.tailSeparator: Function only accepts IList type."
