open Stdio

let stream_fold f init stream =
  let result = ref init in
  Stream.iter
    (fun x -> result := f x !result)
    stream;
  !result

(* Part 1 *)
let required_fuel mass = (mass / 3) - 2

(* Part 2 *)
let required_fuel_rec module_mass =
  let rec go mass total_mass =
    let fuel_mass = required_fuel mass in
    if fuel_mass < 0 then total_mass else go fuel_mass (total_mass + fuel_mass) in
  go module_mass 0

let () =
  let input_stream =
    Stream.from
      (fun _ ->
        try Some (int_of_string (input_line stdin)) with End_of_file -> None) in

  let total = stream_fold (fun x acc -> required_fuel_rec x + acc) 0 input_stream in
  printf "%d\n" total
