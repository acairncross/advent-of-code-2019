open Base
open Stdio

let consecutive_pairs_exn xs =
  List.zip_exn (List.tl_exn xs) (List.drop_last_exn xs)

let contains_double_exn ds =
  List.exists ~f:(fun (x,y) -> x = y) (consecutive_pairs_exn ds)

let contains_exact_double_exn ds =
  let open Base.Container.Continue_or_stop in
  let next_elem (x, count) y =
    if x = y
    then
      Continue (x, count+1)
    else
      match count with
      | 2 -> Stop true
      | _ -> Continue (y, 1)
  in List.fold_until
       ~init:(List.hd_exn ds, 1)
       ~f:next_elem
       ~finish:(fun (_, count) -> count = 2)
       (List.tl_exn ds)

let rec zip3_exn xs ys zs =
  match xs, ys, zs with
  | x :: xs', y :: ys', z :: zs' -> (x,y,z) :: zip3_exn xs' ys' zs'
  | [], [], [] -> []
  | _ -> failwith "list sizes do not match"

let consecutive_triples_exn xs =
  let open List in
  zip3_exn (tl_exn (tl_exn xs)) (tl_exn (drop_last_exn xs)) (drop_last_exn (drop_last_exn xs))

let contains_triple ds =
  List.exists ~f:(fun (x,y,z) -> x = y && y = z) (consecutive_triples_exn ds)

let is_nondecreasing ds =
  List.for_all ~f:(fun (x,y) -> x >= y) (consecutive_pairs_exn ds)

let rec solve hi n total =
  let digits = List.map ~f:Char.to_int (String.to_list (Int.to_string n)) in
  if contains_exact_double_exn digits && is_nondecreasing digits
  then begin Out_channel.printf "%d\n" n; solve hi (n+1) (total+1) end
  else if n = hi
  then total
  else solve hi (n+1) total

let () =
  Out_channel.printf "%d\n" (solve 576723 109165 0)
