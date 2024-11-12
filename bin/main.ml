module type MySet = sig
  type 'a t
  (** ['a t] is the type of sets whose elements are of type ['a]. *)

  val empty : 'a t
  (** [empty] is the empty set. *)

  val mem : 'a -> 'a t -> bool
  (** [mem x s] is whether [x] is an element of [s]. *)

  val add : 'a -> 'a t -> 'a t
  (** [add x s] is the set that contains [x] and all the elements of [s]. *)

  val del : 'a -> 'a t -> 'a t
  (** [del x s] is the set that remove [x] from [s]. *)
end

module RBTSet : MySet = struct
  type 'a t = 'a Rbt.rbtree

  let empty = Rbt.empty
  let mem = Rbt.mem
  let add = Rbt.insert
  let del = Rbt.delete
end

let basic_test () =
  let set = ref RBTSet.empty in
  for i = 0 to 100 do
    set := RBTSet.add i !set;
    assert (RBTSet.mem i !set)
  done;
  for i = 100 downto 0 do
    set := RBTSet.del i !set;
    assert (not (RBTSet.mem i !set))
  done

let rec insert_seq n set =
  match n with
  | 0 -> set
  | _ -> insert_seq (n - 1) (RBTSet.add n set)

let rec insert_rng n set =
  match n with
  | 0 -> set
  | _ -> insert_rng (n - 1) (RBTSet.add (Random.int 100000) set)

let () =
  (* Correctness test *)
  basic_test ();
  (* Performance test for sequential insertion *)
  let t0 = Benchmark.make 0L in
  let _ = insert_seq 100000 RBTSet.empty in
  let e0 = Benchmark.sub (Benchmark.make 0L) t0 in
  print_endline "Sequenctial insert Benchmark Result:";
  print_endline (Benchmark.to_string e0);
  (* Performance test for random insertion *)
  let t1 = Benchmark.make 0L in
  let set = insert_rng 100000 RBTSet.empty in
  let e1 = Benchmark.sub (Benchmark.make 0L) t1 in
  print_endline "Randomize insert Benchmark Result:";
  print_endline (Benchmark.to_string e1);
  (* Performance test for deletion *)
  Benchmark.tabulate
    (Benchmark.latency1 (Int64.of_int 100000) ?name:(Some "Delete performance")
       (fun x -> RBTSet.del (Random.int x) set |> ignore)
       100000)
