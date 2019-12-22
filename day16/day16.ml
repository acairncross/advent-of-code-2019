open Base
open Stdio

let rec apply_n n f x =
  if n = 0 then x else apply_n (n-1) f (f x)

(* Part 1 *)

let base_pattern = [|0;1;0;-1|]

let zip_with_pattern input k =
  (* 0 <= i < 4, 0 <= j < k *)
  let rec go input i j =
    match input with
    | [] -> []
    | hd :: tl ->
       let j' = (j + 1) % k in
       let i' = (i + (j + 1) / k) % 4 in
       (hd, base_pattern.(i)) :: go tl i' j' in
  go input (if k = 1 then 1 else 0) (if k = 1 then 0 else 1)

let fft_phase input =
  List.mapi
    input
    ~f:(fun i _ ->
      zip_with_pattern input (i+1)
      |> List.sum (module Int) ~f:(fun (x, y) -> x * y)
      |> fun x -> abs x % 10)

(* Part 2 *)

let process_input base_input input_reps offset =
  let input_length = Array.length base_input * input_reps in
  let input = Array.create 0 ~len:(input_length - offset) in
  for i = offset to input_length - 1 do
    input.(i - offset) <- base_input.(i % Array.length base_input)
  done;
  input

let special_fft_phase input =
  let length = Array.length input in
  let output = Array.create 0 ~len:length in
  output.(length - 1) <- input.(length - 1);
  for i = length - 2 downto 0 do
    output.(i) <- (output.(i+1) + input.(i)) % 10
  done;
  output

let () =
  let line = In_channel.input_line_exn stdin in
  let input_signal_chars =
    line 
    |> String.strip
    |> String.to_list in
  let input = List.map ~f:Char.get_digit_exn input_signal_chars in

  (* Part 1 *)
  let output1 = apply_n 100 fft_phase input in
  let solution1 = List.take output1 8 in
  Out_channel.printf "solution1: ";
  List.iter solution1 ~f:(fun x -> Out_channel.printf "%d" x);
  Out_channel.printf "\n";

  let offset =
    input_signal_chars
    |> (fun xs -> List.take xs 7)
    |> String.of_char_list
    |> Int.of_string in

  let special_input =
    input
    |> Array.of_list
    |> (fun input -> process_input input 10000 offset) in
  let output2 = apply_n 100 special_fft_phase special_input in

  Out_channel.printf "solution2: ";
  for i = 0 to 7 do
    Out_channel.printf "%d" output2.(i);
  done;
  Out_channel.printf "\n"
