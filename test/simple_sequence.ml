open Simple_sequence

module Sequence_test (S : Sequence) =
  struct
    let rec list_of sequence =
      match S.get sequence with
        | None -> []
        | Some (x, xs) -> x :: list_of xs

    let test_of_list () =
      assert (list_of (S.of_list [1;2;3;4;5]) = [1;2;3;4;5])

    let test_map () =
      assert (list_of (S.map string_of_int (S.of_list [1;2;3;4]))
                = ["1"; "2"; "3"; "4"]) ;
      assert (list_of (S.map (fun x -> x * x) (S.nil ())) = [])

    let test_filter () =
      assert (list_of (S.filter (fun x -> 0 = x mod 2)
                        (S.of_list [1;2;3;4;5;6])) = [2;4;6])

    let test_map_filter () =
      assert (list_of (S.map_filter (fun x ->
                if x mod 2 = 0 then Some (x / 2) else None)
                  (S.of_list [1;3;5;7;9;4;42])) = [2;21])

    let test_append () =
      let open S in
      assert (list_of (
        append
          (cons (fun () -> (1, (cons (fun () -> (2, (nil ())))))))
          (cons (fun () -> (3, (cons (fun () -> (4, (nil ())))))))) =
          [1; 2; 3; 4]) ;
      assert (list_of (append (nil ())
                        (cons (fun () -> (1, (nil ()))))) = [1]) ;
      assert (list_of (append (of_list [3; 4]) (nil ())) = [3; 4])

    let test_flatten () =
      let open S in
      assert (list_of (flatten (
        of_list [of_list [1;2]; of_list [3;4]])) = [1;2;3;4])

    let test_fold_map () =
      assert (list_of
        (S.fold_map (fun x y -> ((x, y), x + y))
            (S.of_list [1;2;3]) 0) = [(1,0); (2,1); (3,3)]) ;
      assert (list_of
        (S.fold_map (fun x y -> ((x, y), x + y))
            (S.of_list []) 0) = [])

    let test_fold () =
      assert (S.fold (fun x acc -> acc ^ x)
                (S.of_list ["a"; "b"; "c"]) "" = "abc")

    let test_reverse () =
      assert (list_of (S.reverse (S.of_list [1;2;3;4])) = [4;3;2;1]) ;
      assert (list_of (S.reverse (S.nil ())) = [])

    let test_from_fun () =
      let i = ref 0 in
      let f () =
        if !i > 9 then None
        else
         (incr i ;
          Some !i) in
      assert (list_of (S.from_fun f) = [1;2;3;4;5;6;7;8;9;10])

    let test () =
      test_of_list () ;
      test_map () ;
      test_filter () ;
      test_map_filter () ;
      test_append () ;
      test_flatten () ;
      test_fold_map () ;
      test_fold () ;
      test_reverse () ;
      test_from_fun ()
  end

module Array_sequence_parameters =
  struct
    let allow_copy_on_resize = true
    let allow_copy_on_multiple_cons = true
  end
module Array_always_copy = Array_sequence (Array_sequence_parameters)
module Array_sequence_test = Sequence_test (Array_always_copy)
module List_sequence_test = Sequence_test (List_sequence)
module Lazy_list_sequence_test = Sequence_test (Lazy_list_sequence) ;;

module Copy_on_resize =
  struct
    let allow_copy_on_resize = true
    let allow_copy_on_multiple_cons = false
  end

module Copy_on_multiple_cons =
  struct
    let allow_copy_on_resize = false
    let allow_copy_on_multiple_cons = true
  end

module Dont_copy =
  struct
    let allow_copy_on_resize = false
    let allow_copy_on_multiple_cons = false
  end

module Array_copy_on_resize = Array_sequence (Copy_on_resize)
module Array_copy_on_multiple_cons = Array_sequence (Copy_on_multiple_cons)
module Array_dont_copy = Array_sequence (Dont_copy)

exception E of string
let test_copy_on_resize () =
  let module S = Sequence_test (Array_copy_on_resize) in
  let a = Array_copy_on_resize.init [| 1; 2 |] in
  let b = Array_copy_on_resize.cons (fun () -> (3, a)) in
  assert (S.list_of b = [3; 1; 2]) ; (* resize works *)
  let _ = Array_copy_on_resize.cons (fun () -> (4, b)) in
  try
    ignore (Array_copy_on_resize.cons (fun () -> (5, b))) ;
    raise (E "Multiple calls of cons on b should fail!") (* multiple cons shouldn't work *)
  with Invalid_argument e -> assert ("Array_sequence.cons: repeated cons!" = e)
    | E e -> failwith e ;;

let test_copy_on_multiple_cons () =
  let module S = Sequence_test (Array_copy_on_multiple_cons) in
  let a = Array_copy_on_multiple_cons.nil_with_capacity 2 0 in
  let b = Array_copy_on_multiple_cons.cons (fun () -> (3, a)) in
  assert (S.list_of b = [3]) ;
  let c = Array_copy_on_multiple_cons.cons (fun () -> (4, b)) in
  assert (S.list_of c = [4; 3]) ;
  assert (S.list_of (Array_copy_on_multiple_cons.cons (fun () -> (5, b))) = [5; 3]) ; (* multiple cons works *)
  try
    ignore (Array_copy_on_multiple_cons.cons (fun () -> (6, c))) ; (* resize shouldn't work *)
    raise (E "Array resizing shouldn't happen with a single cons!")
  with Invalid_argument e -> assert ("Array_sequence.cons: array too small!" = e)
    | E e -> failwith e ;;

let test_dont_copy () =
  let module S = Sequence_test (Array_dont_copy) in
  let a = Array_dont_copy.nil_with_capacity 2 0 in
  let b = Array_dont_copy.cons (fun () -> (3, a)) in
  assert (S.list_of b = [3]) ;
  let c = Array_dont_copy.cons (fun () -> (4, b)) in
  assert (S.list_of c = [4; 3]) ; (* cons without resize works *)
  (try
    ignore (Array_dont_copy.cons (fun () -> (5, b))) ;
    raise (E "Multiple calls of cons on b should fail!") (* multiple cons shouldn't work *)
  with Invalid_argument e -> assert ("Array_sequence.cons: repeated cons!" = e)
    | E e -> failwith e) ;
  try
    ignore (Array_dont_copy.cons (fun () -> (6, c))) ; (* resize shouldn't work *)
    raise (E "Array resizing shouldn't happen with a single cons!")
  with Invalid_argument e -> assert ("Array_sequence.cons: array too small!" = e)
    | E e -> failwith e ;;

let test_nth_from_last () =
  let a = Array_always_copy.init [| 2; 1 |] in
  let a = Array_always_copy.cons (fun () -> (3, a)) in
  assert (Array_always_copy.nth_from_last a (-1) = None) ;
  assert (Array_always_copy.nth_from_last a 3 = None) ;
  assert (Array_always_copy.nth_from_last a 0 = Some 1) ;
  assert (Array_always_copy.nth_from_last a 1 = Some 2) ;
  assert (Array_always_copy.nth_from_last a 2 = Some 3) ;;

let test_to_string_and_of_string () =
  let a = Array_always_copy.of_string "1234" in
  assert (Array_always_copy.nth_from_last a 0 = Some '4') ;
  assert (Array_always_copy.nth_from_last a 1 = Some '3') ;
  assert (Array_always_copy.nth_from_last a 2 = Some '2') ;
  assert (Array_always_copy.nth_from_last a 3 = Some '1') ;
  let b = Array_always_copy.to_string a in
  assert (b = "1234") ;;

let test_to_list_and_length () =
  let a = Array_always_copy.of_string "abc" in
  let b = Array_always_copy.nil () in
  assert (Array_always_copy.to_list a = ['a'; 'b'; 'c']) ;
  assert (Array_always_copy.to_list b = []) ;
  assert (Array_always_copy.length a = 3) ;
  assert (Array_always_copy.length b = 0) ;;

Array_sequence_test.test () ;;
List_sequence_test.test () ;;
Lazy_list_sequence_test.test () ;;
test_copy_on_resize () ;;
test_copy_on_multiple_cons () ;;
test_dont_copy () ;;
test_nth_from_last () ;;
test_to_string_and_of_string () ;;
test_to_list_and_length () ;;
