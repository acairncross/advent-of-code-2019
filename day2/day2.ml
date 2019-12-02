let rec eval program pc =
  match program.(pc) with
  | 1 ->
     program.(program.(pc+3)) <- program.(program.(pc+1)) + program.(program.(pc+2));
     eval program (pc+4)
  | 2 ->
     program.(program.(pc+3)) <- program.(program.(pc+1)) * program.(program.(pc+2));
     eval program (pc+4)
  | 99 -> program.(0)
  | _ -> failwith "unexpected opcode"

(* Part 1 *)
let initialize_program program =
  program.(1) <- 12;
  program.(2) <- 2

(* Part 2 *)
let find_initial_state program target =
  let rec go n m =
    let program' = Array.copy program in
    (* Printf.printf "%d %d\n" n m; *)
    program'.(1) <- n;
    program'.(2) <- m;
    if try eval program' 0 == target with Invalid_argument _ -> false
    then (n, m)
    else
      if (n, m) = (99, 99)
      then failwith "no answer found"
      else go (n + (m+1)/99) ((m+1) mod 99) in
  go 0 0

let () =
  let program = Array.of_list (List.map int_of_string (String.split_on_char ',' (String.trim (Std.input_all stdin)))) in
  let (n, m) = find_initial_state program 19690720 in
  Printf.printf "%d\n" (100 * n + m)
