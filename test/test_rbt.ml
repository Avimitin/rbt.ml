open OUnit2
open Rbt

let populate cnt =
  let rec ins xs = function
    | 0 -> xs
    | n -> ins (insert n xs) (n - 1)
  in
  ins empty cnt

let is_bst _ =
  let set = populate 100 in
  let rec check = function
    | Leaf -> true
    | LeafBB -> true
    | Node (_, Node (color, a, y, b), x, _) ->
        if x < y then
          false
        else
          check (Node (color, a, y, b))
    | Node (_, _, x, Node (color, c, y, d)) ->
        if x > y then
          false
        else
          check (Node (color, c, y, d))
    | Node (_, Leaf, _, Leaf)
    | Node (_, LeafBB, _, LeafBB)
    | Node (_, Leaf, _, LeafBB)
    | Node (_, LeafBB, _, Leaf) ->
        true
  in
  check set |> assert_bool "Set is not a BST"

let is_234 _ =
  let set = populate 100 in
  let rec check = function
    | Node (Red, Node (Red, _, _, _), _, _) -> false
    | Node (Red, _, _, Node (Red, _, _, _)) -> false
    | Node (_, Node (c, a, x, b), _, _) -> check (Node (c, a, x, b))
    | Node (_, _, _, Node (c, a, x, b)) -> check (Node (c, a, x, b))
    | Node (_, Leaf, _, Leaf)
    | Node (_, LeafBB, _, LeafBB)
    | Node (_, Leaf, _, LeafBB)
    | Node (_, LeafBB, _, Leaf) ->
        true
    | Leaf -> true
    | LeafBB -> true
  in
  check set |> assert_bool "Set is not follow color rules"

let is_balanced _ =
  let set = populate 100 in
  let rec left_most_path_black_count acc = function
    | Leaf | LeafBB -> acc
    | Node (c, lft, _, _) ->
        left_most_path_black_count
          (match c with
          | Black -> acc + 1
          | _ -> acc)
          lft
  in
  let check = function
    | Leaf | LeafBB -> true
    | Node (_, left, _, right) ->
        let cmp = left_most_path_black_count 0 left in
        let rec aux acc = function
          | Leaf | LeafBB -> acc = cmp
          | Node (c, lft, _, rgt) ->
              let acc =
                match c with
                | Black -> acc + 1
                | _ -> acc
              in
              aux acc lft && aux acc rgt
        in
        aux 0 left && aux 0 right
  in
  check set |> assert_bool "Set is not balanced"

(* Name the test cases and group them together *)
let suite =
  "suite"
  >::: [ "isBST" >:: is_bst; "is234" >:: is_234; "isBalanced" >:: is_balanced ]

let () = run_test_tt_main suite
