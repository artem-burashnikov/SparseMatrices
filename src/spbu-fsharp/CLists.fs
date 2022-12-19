module CLists

type CList<'Value> =
    | Cons of head: 'Value * tail: CList<'Value>
    | Empty

let rec map f lst =
    match lst with
    | Empty -> Empty
    | Cons(hd, tl) -> Cons(f hd, map f tl)

let rec fold folder acc lst =
    let recurse = fold folder

    match lst with
    | Empty -> acc
    | Cons(head, tail) -> recurse (folder acc head) tail

let go () = map ((+) 1) (Cons(1, Cons(3, Empty)))

let _go () = map ((-) 1) (Cons(1, Cons(3, Empty)))

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
