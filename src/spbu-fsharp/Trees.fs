module Tree


type BinTree<'value> =
    | FullNode of value: 'value * left: BinTree<'value> * right: BinTree<'value>
    | NodeWithLeft of value:'value * left: BinTree<'value>
    | NodeWithRight of value:'value * right: BinTree<'value>
    | Leaf of value:'value


let rec minInTree tree =
    match tree with
    | Leaf(value) -> value
    | FullNode(value, leftNode, rightNode) ->
        let minLeft = minInTree leftNode
        let minRight = minInTree rightNode
        min value (min minLeft minRight)
    | NodeWithLeft(value, left) ->
        let minLeft = minInTree left
        min value minLeft
    | NodeWithRight(value, right) ->
        let minRight = minInTree right
        min value minRight


