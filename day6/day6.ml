open Base
open Stdio

type 'a tree = Node of 'a * 'a tree list

let add_to_child_map child_map parent child =
  Map.update
    child_map
    parent
    ~f:(function
      | None -> [child]
      | Some(children) -> child :: children)

let read_input =
  In_channel.fold_lines
    stdin
    ~init:(Map.empty (module String))
    ~f:(fun child_map line ->
      match String.strip line |> String.split ~on:')' with
      | [parent; child] -> add_to_child_map child_map parent child
      | _ -> failwith "failed to parse input")

let rec child_map_to_tree child_map parent =
  let children = match Map.find child_map parent with
  | None -> []
  | Some(child) -> child in
  let children_trees = List.map ~f:(fun p -> child_map_to_tree child_map p) children in
  Node(parent, children_trees)

let rec count_orbits (Node(_, ts)) num_ancestors =
  match ts with
  | [] -> num_ancestors
  | ts ->
     List.sum
       (module Int)
       ~f:(fun t -> count_orbits t (num_ancestors+1))
       ts
     + num_ancestors

let rec find_some_in_list = function
  | [] -> None
  | None :: tl -> find_some_in_list tl
  | Some(x) :: _ -> Some(x)

let rec find_in_tree (Node(x, ts)) y =
  if String.equal y x
  then Some([])
  else
    let routes = List.map ~f:(fun t -> find_in_tree t y) ts in
    Option.map ~f:(fun xs -> x :: xs) (find_some_in_list routes)

let rec common_prefix xs ys =
  match xs, ys with
  | x :: xs, y :: ys -> if String.equal x y then x :: common_prefix xs ys else []
  | _, _ -> []

let _ =
  let orbit_tree = read_input |> fun child_map -> child_map_to_tree child_map "COM" in
  let num_orbits = count_orbits orbit_tree 0 in
  Out_channel.printf "%d\n" num_orbits;
  match
    let open Option in
    find_in_tree orbit_tree "YOU" >>= fun r1 ->
    find_in_tree orbit_tree "SAN" >>= fun r2 ->
    let prefix_length = List.length (common_prefix r1 r2) in
    return ((List.length r1 - prefix_length) + (List.length r2 - prefix_length))
  with
  | None -> Out_channel.printf "Failed\n"
  | Some(n) -> Out_channel.printf "%d\n" n
