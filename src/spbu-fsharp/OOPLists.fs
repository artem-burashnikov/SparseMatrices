module OOPLists

type IList<'Value> =
    interface
    end

type List<'Value>(head: 'Value, tail: IList<'Value>) =
    interface IList<'Value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'Value>() =
    interface IList<'Value>

/// Count the number of elements in a list (IList)
let rec getLength (lst: IList<'Value>) : int =
    match lst with
    | :? EmptyList<'Value> -> 0
    | :? List<'Value> as lst -> 1 + getLength lst.Tail
    | _ ->
        failwith
            "OOPLists.getLength: Function only accepts IList types. \
                    Incorrect variable type was given."

/// Takes a linked list cell and returns its value.
let headSeparator (lst: IList<'Value>) : 'Value =
    match lst with
    | :? List<'Value> as lst -> lst.Head
    | _ -> failwith "OOPLists.headSeparator: Function only accepts List type."

// Takes a linked list cell and returns the next cell linked to it.
let tailSeparator (lst: IList<'Value>) : IList<'Value> =
    match lst with
    | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
    | :? List<'Value> as lst -> lst.Tail
    | _ -> failwith "OOPLists.tailSeparator: Function only accepts IList type."
