open Base
open Stdio

let div_ru x y = if Int.rem x y = 0 then x/y else x/y + 1

let reverse_dependencies reactions =
  let add_mapping map key value =
    Map.update map key ~f:(function
        | None -> Set.singleton (module String) value
        | Some(values) -> Set.add values value) in
  let rec go reactions rev_deps =
    match reactions with
    | [] -> rev_deps
    | (reactants, product) :: reactions ->
       let rev_deps' =
         List.fold_left
           reactants
           ~init:rev_deps
           ~f:(fun map r -> add_mapping map r product) in
       go reactions rev_deps'
  in go reactions (Map.singleton (module String) "FUEL" (Set.empty (module String)))

let find_undepended rev_deps_orig rev_deps =
  let rec go = function
    | [] -> None
    | (product, dependants) :: tl ->
       if Set.is_empty dependants
       then Some(product, Map.find_exn rev_deps_orig product)
       else go tl
  in go (Map.to_alist rev_deps)

(* Find how much of the reactant we need to make dependant_amount of the dependant *)
let find_required_amount reactions_list reactant dependant dependant_amount =
  let rec find_reaction = function
    | [] -> failwith "couldn't find reaction"
    | (_reactants, (_product_n, product_name)) as reaction :: tl ->
       if String.equal product_name dependant
       then reaction
       else find_reaction tl in
  let (reactants, (product_n, _)) = find_reaction reactions_list in
  let rec find_reactant_n = function
    | [] -> failwith "couldn't find reactant"
    | (reactant_n, reactant_name) :: tl ->
       if String.equal reactant_name reactant
       then reactant_n
       else find_reactant_n tl in
  let reactant_n = find_reactant_n reactants in
  (div_ru dependant_amount product_n) * reactant_n

let rec solve reactions_list rev_deps_orig rev_deps required_amounts fuel_amount =
  if Map.is_empty rev_deps
  then Map.find_exn required_amounts "ORE"
  else
    match find_undepended rev_deps_orig rev_deps with
    | None -> failwith "couldn't find undepended reactant"
    | Some(reactant, dependants) ->
       let rev_deps' =
         Map.remove rev_deps reactant
         |> Map.map ~f:(fun dependants -> Set.remove dependants reactant) in
       let required_amount' =
         if String.equal reactant "FUEL" then fuel_amount
         else
           Set.sum (module Int)
             ~f:(fun dependant ->
               find_required_amount
                 reactions_list
                 reactant
                 dependant
                 (Map.find_exn required_amounts dependant))
             dependants
         in
       let required_amounts' =
         Map.add_exn
           required_amounts
           ~key:reactant
           ~data:required_amount' in
       solve reactions_list rev_deps_orig rev_deps' required_amounts' fuel_amount

(* Find the largest number (n) between lo and hi such that pred n holds.
 * e.g. The largest amount of fuel such that it can be made with some amount of ore.
 * Invariant: lo <= n <= hi *)
let binary_search (pred : int -> bool) (lo : int) (hi : int) =
  let rec go lo hi =
    if lo = hi
    then lo
    else
      let m = (lo + hi) / 2 in
      if pred m
      then go lo m
      else go m (hi-1)
  in go lo hi

let () =
  let reactions_list =
    let lexbuf = Lexing.from_channel stdin in
    Parser.reactions Lexer.read lexbuf in
  let unquantified_reactions =
    List.map
      reactions_list
      ~f:(fun (reactants, product) -> List.map reactants ~f:snd, snd product) in
  let rev_deps = reverse_dependencies unquantified_reactions in
  let solution = solve reactions_list rev_deps rev_deps (Map.empty (module String)) 1 in
  Out_channel.printf "Solution 1: %d\n" solution;
  let ore = 1000000000000 in
  let predicate fuel =
    solve reactions_list rev_deps rev_deps (Map.empty (module String)) fuel > ore in
  let fuel_upper_bound = ore in
  let solution2 =
    binary_search predicate 0 fuel_upper_bound in
  Out_channel.printf "Solution 2: %d\n" solution2
