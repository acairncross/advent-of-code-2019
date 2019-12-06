open Base
open Stdio

module Int_pair_comparator = struct
  type t = int * int
  include Comparator.Make (struct type t = int * int [@@deriving sexp, compare] end)
end

let rec trace_wire_path_1 instrs (x, y) locs =
  match instrs with
  | instr :: instrs' ->
     let cmp = Set.comparator_s locs in
     let dir = Char.of_string (String.prefix instr 1) in
     let dist = Int.of_string (String.drop_prefix instr 1) in
     begin match dir with
     | 'R' ->
        let next_locs = List.map ~f:(fun x' -> (x', y)) (List.range ~start:`exclusive ~stop:`inclusive x (x+dist))
        in trace_wire_path_1 instrs' (x+dist, y) (Set.union locs (Set.of_list cmp next_locs))
     | 'U' ->
        let next_locs = List.map ~f:(fun y' -> (x, y')) (List.range ~start:`exclusive ~stop:`inclusive y (y+dist))
        in trace_wire_path_1 instrs' (x, y+dist) (Set.union locs (Set.of_list cmp next_locs))
     | 'L' ->
        let next_locs = List.map ~f:(fun x' -> (x', y)) (List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive x (x-dist))
        in trace_wire_path_1 instrs' (x-dist, y) (Set.union locs (Set.of_list cmp next_locs))
     | 'D' ->
        let next_locs = List.map ~f:(fun y' -> (x, y')) (List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive y (y-dist))
        in trace_wire_path_1 instrs' (x, y-dist) (Set.union locs (Set.of_list cmp next_locs))
     | _ -> failwith "unexpected instruction"
     end
  | [] -> locs

let rec trace_wire_path_2 instrs (x, y) locs n =
  match instrs with
  | instr :: instrs' ->
     let dir = Char.of_string (String.prefix instr 1) in
     let dist = Int.of_string (String.drop_prefix instr 1) in
     let add_to_map m xs = List.fold_right xs ~f:(fun ((x', y'), i) m' -> if Map.mem m' (x', y') then m' else Map.add_exn m' ~key:(x', y') ~data:i) ~init:m in
     begin match dir with
     | 'R' ->
        let next_locs = List.map ~f:(fun x' -> (x', y)) (List.range ~start:`exclusive ~stop:`inclusive x (x+dist)) in
        let next_locs' = List.zip_exn next_locs (List.range n (n+dist))
        in trace_wire_path_2 instrs' (x+dist, y) (add_to_map locs next_locs') (n+dist)
     | 'U' ->
        let next_locs = List.map ~f:(fun y' -> (x, y')) (List.range ~start:`exclusive ~stop:`inclusive y (y+dist)) in
        let next_locs' = List.zip_exn next_locs (List.range n (n+dist))
        in trace_wire_path_2 instrs' (x, y+dist) (add_to_map locs next_locs') (n+dist)
     | 'L' ->
        let next_locs = List.map ~f:(fun x' -> (x', y)) (List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive x (x-dist)) in
        let next_locs' = List.zip_exn next_locs (List.range n (n+dist))
        in trace_wire_path_2 instrs' (x-dist, y) (add_to_map locs next_locs') (n+dist)
     | 'D' ->
        let next_locs = List.map ~f:(fun y' -> (x, y')) (List.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive y (y-dist)) in
        let next_locs' = List.zip_exn next_locs (List.range n (n+dist))
        in trace_wire_path_2 instrs' (x, y-dist) (add_to_map locs next_locs') (n+dist)
     | _ -> failwith "unexpected instruction"
     end
  | [] -> locs

let () =
  let wire1 = String.split ~on:',' (String.strip (Option.value_exn (In_channel.input_line stdin))) in
  let wire2 = String.split ~on:',' (String.strip (Option.value_exn (In_channel.input_line stdin))) in
  let wire_locs1 = trace_wire_path_2 wire1 (0, 0) (Map.empty (module Int_pair_comparator)) 1 in
  let wire_locs2 = trace_wire_path_2 wire2 (0, 0) (Map.empty (module Int_pair_comparator)) 1 in
  (* Map.iteri ~f:(fun ~key:(x, y) ~data:i -> Out_channel.printf "%d: (%d, %d)\n" i x y) wire_locs1 *)
  (* List.iter ~f:(fun (x, y) -> Out_channel.printf "(%d, %d)\n" x y) (List.sort ~compare:(fun (x1,y1) (x2,y2) -> compare (abs x1 + abs y1) (abs x2 + abs y2)) (Set.to_list (Set.inter wire_locs1 wire_locs2))) *)
  let intersections = Set.to_list (Set.inter (Set.of_list (module Int_pair_comparator) (Map.keys wire_locs1)) (Set.of_list (module Int_pair_comparator) (Map.keys wire_locs2))) in
  let score (x, y) = Map.find_exn wire_locs1 (x, y) + Map.find_exn wire_locs2 (x, y) in
  let sorted = List.sort
       ~compare:(fun (x1,y1) (x2,y2) -> compare (score (x1,y1)) (score (x2,y2)))
       intersections in
  List.iter ~f:(fun (x, y) -> Out_channel.printf "(%d, %d): %d\n" x y (score (x, y))) sorted

  (* Set.iter ~f:(fun (x, y) -> Out_channel.printf "(%d, %d)\n" x y) wire_locs1 *)
  (* List.iter ~f:(fun (x, y) -> Out_channel.printf "(%d, %d)\n" x y) (List.sort ~compare:(fun (x1,y1) (x2,y2) -> compare (abs x1 + abs y1) (abs x2 + abs y2)) (Set.to_list (Set.inter wire_locs1 wire_locs2))) *)
