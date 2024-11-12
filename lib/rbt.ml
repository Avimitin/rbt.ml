(* Red-black trees are an efficient representation of ordered sets, and many
   common operations, such as search and insertion, are possible in logarithmic
   time. Their efficiency stems from their mostly-balanced nature, which is
   guaranteed by their structural invariants. A red-black tree is a binary tree
   in which each node is colored red or black, and whose construction satisfies
   two properties:

   1. Local Invariant Properties: There are no two adjacent red nodes along any
   path.
   2. Global Invariant Properties: Every path from the root to a leaf has
   the same number of black nodes. This number is called the black height (BH)
   of the tree.

   For efficient deletion purposes, leaf nodes do not house a value and are
   colored black.*)

(** Color for an Red Black Tree node

    DoubleBlack node is used for optimizing the [delete] operation. As it's
    name, each path that is colored with [DoubleBlack] have Black Height of 2. *)
type color = Red | Black | DoubleBlack

(** Type of an Red Black Tree.
    [Leaf] indicates that current node have no value.
    Each [Node] contains ([Color], Left Tree, value, Right Tree).
    And [LeafBB] is used internally for [delete] operation *)
type 'a rbtree = Leaf | Node of (color * 'a rbtree * 'a * 'a rbtree) | LeafBB

(** [empty] is a [Leaf] node *)
let empty = Leaf

(** [mem] return true if the [x] is a member of the given rbtree *)
let rec mem x = function
  | Node (_, left, cur, right) ->
      if x < cur then
        mem x left
      else if x > cur then
        mem x right
      else
        true
  | _ -> false

(** [init] return a new rbtree with the given value as root node, colored with [Black], and have two [Leaf] node. *)
let init x = Node (Black, Leaf, x, Leaf)

(** [balance] rotate the given [Node] to fix the violation of Red-Black-Tree invariants.
    This function will return same [Node] back if the given [Node] doesn't
    match the above 4 cases.*)
let balance = function
  (* There are four possible cases that could violate "Local Invariant" when
     inserting a [Red] node.
     (R indicates [Red], B indicates [Black], a < x < b < y < c < z < d)

            1             2             3             4

            Bz            Bz            Bx            Bx
           / \           / \           / \           / \
          Ry  d         Rx  d         a   Rz        a   Ry
         /  \          / \               /  \          /  \
      [Rx]  c         a  [Ry]          [Ry]  d        b   [Rz]
      /  \               /  \          / \                /  \
     a    b             b    c        b   c              c    d

     After rotation, the returning [Node] will have below hierarchy, fixing all
     invariant rules and preseve the element comparison rule.

          Ry
         /  \
       Bx    Bz
      / \   / \
     a   b c   d

     After [delete], [balance] might met the following tree arrangement:

             1             2
            BBz           BBx
            / \           / \
           Rx  d         a  [Rz]
          /  \              / \
         a   Ry            Ry  d
            /  \          /  \
           b    c        b    c

     All of them should be converted into the below arrangement:

          By
         /  \
       Bx    Bz
      / \   / \
     a   b c   d

     This function will return same [Node] back if the given [Node] doesn't
     match the above cases.
  *)
  | Node (Black, Node (Red, Node (Red, a, x, b), y, c), z, d)
  | Node (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d)
  | Node (Black, a, x, Node (Red, Node (Red, b, y, c), z, d))
  | Node (Black, a, x, Node (Red, b, y, Node (Red, c, z, d))) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | Node (DoubleBlack, Node (Red, a, x, Node (Red, b, y, c)), z, d)
  | Node (DoubleBlack, a, x, Node (Red, Node (Red, b, y, c), z, d)) ->
      Node (Black, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | tree -> tree

(** [insert] the given [v]alue to the given [tree]. If the given node is a [Leaf] node, then a new rbtree root is returned.
    If the given [v]alue exists in the given [tree], then the original [tree] is returned.

    Okasaki, C. (1999) Red-black trees in a functional setting. J. Funct. Program. 9(04), 471–477*)
let insert v tree =
  let rec ins_rec = function
    | Node (color, left, cur, right) as node ->
        if v < cur then
          Node (color, ins_rec left, cur, right) |> balance
        else if v > cur then
          Node (color, left, cur, ins_rec right) |> balance
        else
          node
    | _ -> Node (Red, Leaf, v, Leaf)
  in
  match ins_rec tree with
  | Node (_, left, root, right) -> Node (Black, left, root, right)
  | Leaf | LeafBB -> failwith "RBT insert failed with ins returning leaf"

(* After introducing the [DoubleBlack] colored node, there are only three
   tree arrangement that needs double-black node treatment.

   For each arrangement we have the following tree rotation

     1.

          Ry                    Bz
         /  \                  / \
       BBx  Bz      ->        Ry  d
      / \   / \              /  \
     a   b c   d          [Rx]  c
                          /  \
                         a    b

     2.
          Ry                Bx
         /  \              / \
       Bx   BBz      ->   a  Ry
      / \   / \             / \
     a   b c   d           b  Bz
                             / \
                            c  d

     3.
          By                   BBz (BBz require [balance] to discharged into Black node)
         /  \                  / \
       BBx  Bz      ->        Ry  d
      / \   / \              /  \
     a   b c   d           Rx  c
                          /  \
                         a    b

     4.
          Bx                   Bz
         /  \                 / \
       BBw   Rz              By  e
      / \    / \    ->      /  \
     a   b  By   e         Rx   d
           / \            /  \
          c   d         Bw    c
                       / \
                      a  b

     5.
          By                   Bw
         /  \                 / \
       Rw    BBz             a   Bx
      / \    / \    ->          /  \
     a  Bx  d   e               b   Ry
       /  \                        /  \
      b    c                      c    Bz
                                       / \
                                      d  e
*)
let rotate =
  (* When the root node is colored with [DoubleBlack], we might met red-red
     violation, and it requires the tree context to discharge double-black node,
     so here we let the [balance] function to help resolve the tree *)
  function
  (* C1: Red(DoubleBlack, y, Black) *)
  | Node (Red, Node (DoubleBlack, a, x, b), y, Node (Black, c, z, d)) ->
      Node (Black, Node (Red, Node (Black, a, x, b), y, c), z, d) |> balance
  | Node (Red, LeafBB, y, Node (Black, c, z, d)) ->
      Node (Black, Node (Red, Leaf, y, c), z, d) |> balance
  (* C2: Red(Black, y, DoubleBlack) *)
  | Node (Red, Node (Black, a, x, b), y, Node (DoubleBlack, c, z, d)) ->
      Node (Black, a, x, Node (Red, b, y, Node (Black, c, z, d))) |> balance
  | Node (Red, Node (Black, a, x, b), y, LeafBB) ->
      Node (Black, a, x, Node (Red, b, y, Leaf)) |> balance
  (* C3: Black(DoubleBlack, y, Black) *)
  | Node (Black, Node (DoubleBlack, a, x, b), y, Node (Black, c, z, d)) ->
      Node (DoubleBlack, Node (Red, Node (Black, a, x, b), y, c), z, d)
      |> balance
  | Node (Black, LeafBB, y, Node (Black, c, z, d)) ->
      Node (DoubleBlack, Node (Red, Leaf, y, c), z, d) |> balance
  (* C4: Black(Black, y, DoubleBlack) *)
  | Node (Black, Node (Black, a, x, b), y, Node (DoubleBlack, c, z, d)) ->
      Node (DoubleBlack, a, x, Node (Red, b, y, Node (Black, c, z, d)))
  | Node (Black, Node (Black, a, x, b), y, LeafBB) ->
      Node (DoubleBlack, a, x, Node (Red, b, y, Leaf))
  (* C5: The least two cases require special handling by introducing a new Black child (which must be true when their parent is a red node) *)
  | Node
      ( Black,
        Node (DoubleBlack, a, w, b),
        x,
        Node (Red, Node (Black, c, y, d), z, e) ) ->
      Node
        ( Black,
          Node (Black, Node (Red, Node (Black, a, w, b), x, c), y, d) |> balance,
          z,
          e )
  | Node (Black, LeafBB, x, Node (Red, Node (Black, c, y, d), z, e)) ->
      Node (Black, Node (Black, Node (Red, Leaf, x, c), y, d) |> balance, z, e)
  (* C6 *)
  | Node
      ( Black,
        Node (Red, a, w, Node (Black, b, x, c)),
        y,
        Node (DoubleBlack, d, z, e) ) ->
      Node
        ( Black,
          a,
          w,
          Node (Black, b, x, Node (Red, c, y, Node (Black, d, z, e))) |> balance
        )
  | Node (Black, Node (Red, a, w, Node (Black, b, x, c)), y, LeafBB) ->
      Node (Black, a, w, Node (Black, b, x, Node (Red, c, y, Leaf)) |> balance)
  | t -> t

(** Turn a double-black sub-tree to red-root tree with two black children.

   After [rotate], a double black tree might exists at the root of the tree.
   But using the [redden] operation, the current [delete] algorithm can
   guarantee that a bubbling double-black node will be discharged before it
   reached the root.*)
let redden = function
  | Node (Black, left, y, right) -> Node (Red, left, y, right)
  | t -> t

(** [delete] the given [v]alue from the given [tree]. If the [tree] is a [Leaf] node, a [Leaf] node is returned.

    Implementation of the paper "Deletion: The Curse of the Red-Black Tree Journal of Functional Programming":
    https://doi.org/10.1017/S0956796814000227 *)
let delete v tree =
  (* Delete inorder successor from right children using recursive [del] is not efficient. *)
  let rec r_del = function
    | Leaf -> failwith "unreachable, [r_del] should not delete a [Leaf]"
    | LeafBB -> failwith "unreachable, [r_del] should not delete a [LeafBB]"
    | Node (Red, Leaf, cur, Leaf) -> (cur, Leaf)
    | Node (Black, Leaf, cur, Leaf) -> (cur, LeafBB)
    | Node (Black, Leaf, cur, Node (Red, Leaf, child, Leaf)) ->
        (cur, Node (Black, Leaf, child, Leaf))
    | Node (color, left, cur, right) ->
        let cur', left' = r_del left in
        (cur', rotate (Node (color, left', cur, right)))
  in
  let rec del = function
    | Leaf -> Leaf
    | LeafBB -> failwith "should not [del] [LeafBB]"
    | Node (Black, Leaf, cur, Leaf) as node ->
        if cur == v then
          LeafBB
        else
          node
    | Node (Red, Leaf, cur, Leaf) as node ->
        if cur == v then
          Leaf
        else
          node
    | Node (Black, Node (Red, Leaf, child, Leaf), cur, Leaf) as node ->
        if v < cur then
          Node (Black, del (Node (Red, Leaf, child, Leaf)), cur, Leaf)
        else if v > cur then
          node
        else
          Node (Black, Leaf, child, Leaf)
    | Node (color, left, cur, right) ->
        if v < cur then
          Node (color, del left, cur, right) |> rotate
        else if v > cur then
          Node (color, left, cur, del right) |> rotate
        else
          let cur', right' = r_del right in
          Node (color, left, cur', right') |> rotate
  in
  redden tree |> del
