open Base
open Stdio

module Int_pair_comparator = struct
  type t = int * int
  include Comparator.Make (struct type t = int * int [@@deriving sexp, compare] end)
end

type program_state =
  int array * int * int (* program, pc, rb *)

type program_status =
  | Paused of int list * program_state
  | Finished of int list

type opcode =
  | AddOp
  | MulOp
  | LoadOp
  | OutputOp
  | JumpIfTrueOp
  | JumpIfFalseOp
  | LessThanOp
  | EqualsOp
  | RelBaseOffsetOp
  | HaltOp

type parameter_mode =
  | PositionMode
  | ImmediateMode
  | RelativeMode

type instruction_header =
  | Add of parameter_mode * parameter_mode * parameter_mode
  | Mul of parameter_mode * parameter_mode * parameter_mode
  | Load of parameter_mode
  | Output of parameter_mode
  | JumpIfTrue of parameter_mode * parameter_mode
  | JumpIfFalse of parameter_mode * parameter_mode
  | LessThan of parameter_mode * parameter_mode * parameter_mode
  | Equals of parameter_mode * parameter_mode * parameter_mode
  | RelBaseOffset of parameter_mode
  | Halt

let opcode_of_int = function
  | 1 -> Some AddOp
  | 2 -> Some MulOp
  | 3 -> Some LoadOp
  | 4 -> Some OutputOp
  | 5 -> Some JumpIfTrueOp
  | 6 -> Some JumpIfFalseOp
  | 7 -> Some LessThanOp
  | 8 -> Some EqualsOp
  | 9 -> Some RelBaseOffsetOp
  | 99 -> Some HaltOp
  | _ -> None

let num_params_of_opcode = function
  | AddOp -> 3
  | MulOp -> 3
  | LoadOp -> 1
  | OutputOp -> 1
  | JumpIfTrueOp -> 2
  | JumpIfFalseOp -> 2
  | LessThanOp -> 3
  | EqualsOp -> 3
  | RelBaseOffsetOp -> 1
  | HaltOp -> 0

let parameter_mode_of_int = function
  | 0 -> Some PositionMode
  | 1 -> Some ImmediateMode
  | 2 -> Some RelativeMode
  | _ -> None

let read_param_value program rb param_mode param =
  match param_mode with
  | PositionMode -> program.(param)
  | ImmediateMode -> param
  | RelativeMode -> program.(rb+param)

let write_param_value program rb param_mode param value =
  match param_mode with
  | PositionMode -> program.(param) <- value
  | ImmediateMode -> () (* Allow it *)
  | RelativeMode -> program.(rb+param) <- value

let (let*) = let open Option in (>>=)

let rec traverse f xs =
  let open Option in
  match xs with
  | hd :: tl ->
     let* y = f hd in
     let* ys = traverse f tl in
     return (y :: ys)
  | [] -> Some []

let parse_header hdr =
  let open Option in
  let (param_modes_str, opcode_str) =
    hdr
    |> Int.to_string
    |> fun hdr_str -> (String.drop_suffix hdr_str 2, String.suffix hdr_str 2) in
  let* opcode =
    opcode_str
    |> Int.of_string
    |> opcode_of_int in
  let num_params = num_params_of_opcode opcode in
  let param_modes_int_list =
    String.make (num_params - String.length param_modes_str) '0' ^ param_modes_str
    |> String.to_list
    |> List.map ~f:(fun char -> Int.of_string (String.of_char char))
    |> List.rev in
  let* param_modes_list = traverse parameter_mode_of_int param_modes_int_list in
  let* instruction_header = match param_modes_list, opcode with
    | [m1;m2;m3], AddOp -> Some (Add (m1, m2, m3))
    | [m1;m2;m3], MulOp -> Some (Mul (m1, m2, m3))
    | [m], LoadOp -> Some (Load m)
    | [m], OutputOp -> Some (Output m)
    | [m1;m2], JumpIfTrueOp -> Some (JumpIfTrue (m1, m2))
    | [m1;m2], JumpIfFalseOp -> Some (JumpIfFalse (m1, m2))
    | [m1;m2;m3], LessThanOp -> Some (LessThan (m1, m2, m3))
    | [m1;m2;m3], EqualsOp -> Some (Equals (m1, m2, m3))
    | [m], RelBaseOffsetOp -> Some (RelBaseOffset m)
    | [], HaltOp -> Some Halt
    | _ -> None in
  return instruction_header

let rec eval program pc rb input =
  let open Option in
  let* instruction_header = parse_header program.(pc) in
  match instruction_header with
    | Add(m1, m2, m3) ->
       (* Out_channel.printf "Add (%d)\n" pc; *)
       evalBinop program pc rb input (+) (m1, m2, m3)
    | Mul(m1, m2, m3) ->
       (* Out_channel.printf "Mul (%d)\n" pc; *)
       evalBinop program pc rb input ( * ) (m1, m2, m3)
    | Load(m) ->
       Out_channel.printf "Load (%d)\n" pc;
       begin match input with
       | hd :: tl ->
          write_param_value program rb m (program.(pc+1)) hd;
          eval program (pc+2) rb tl
       | [] -> Some (Paused ([], (program, pc, rb)))
       end
    | Output(m) ->
       let output = read_param_value program rb m program.(pc+1) in
       Out_channel.printf "Output: %d\n" output;
       let* result = eval program (pc+2) rb input in
       begin match result with
       | Paused (outputs, state) -> return (Paused (output :: outputs, state))
       | Finished outputs -> return (Finished (output :: outputs))
       end
    | JumpIfTrue(m1, m2) ->
       (* Out_channel.printf "JumpIfTrue (%d)\n" pc; *)
       if read_param_value program rb m1 program.(pc+1) = 0
       then eval program (pc+3) rb input
       else eval program (read_param_value program rb m2 program.(pc+2)) rb input
    | JumpIfFalse(m1, m2) ->
       (* Out_channel.printf "JumpIfFalse (%d)\n" pc; *)
       if read_param_value program rb m1 program.(pc+1) = 0
       then eval program (read_param_value program rb m2 program.(pc+2)) rb input
       else eval program (pc+3) rb input
    | LessThan(m1, m2, m3) ->
       (* Out_channel.printf "LessThan (%d)\n" pc; *)
       evalBinop program pc rb input (fun x y -> if x < y then 1 else 0) (m1, m2, m3)
    | Equals(m1, m2, m3) ->
       (* Out_channel.printf "Equals (%d)\n" pc; *)
       evalBinop program pc rb input (fun x y -> if x = y then 1 else 0) (m1, m2, m3)
    | RelBaseOffset(m) ->
       (* Out_channel.printf "RelBaseOffset (%d) (%d)\n" pc rb; *)
       eval program (pc+2) (rb + read_param_value program rb m program.(pc+1)) input
    | Halt ->
       Out_channel.printf "Halt\n";
       return (Finished [])
and evalBinop program pc rb inputOpt op (m1, m2, m3) =
  let operand1 = read_param_value program rb m1 program.(pc+1) in
  let operand2 = read_param_value program rb m2 program.(pc+2) in
  write_param_value program rb m3 (program.(pc+3)) (op operand1 operand2);
  eval program (pc+4) rb inputOpt

let rec expanding_eval program pc rb input =
  try eval (Array.copy program) pc rb input with
  | Invalid_argument(_) ->
     Out_channel.printf "Array was too small, expanding\n";
     let expanded_program = Array.create ~len:(2 * Array.length program) 0 in
     for i = 0 to (Array.length program - 1) do
       expanded_program.(i) <- program.(i)
     done;
     expanding_eval expanded_program pc rb input

let find_with_default map key default =
  match Map.find map key with
  | Some(x) -> x
  | None -> default

let minmax xs =
  let open Option in
  let* min = List.min_elt xs ~compare:Int.compare in
  let* max = List.max_elt xs ~compare:Int.compare in
  return (min, max)

(*
let print_map panels =
  let open Option in
  let keys = Map.keys panels in
  let* (xlo, xhi) = keys |> List.map ~f:fst |> minmax in
  let* (ylo, yhi) = keys |> List.map ~f:snd |> minmax in
  for i = yhi downto ylo do
    for j = xlo to xhi do
      if find_with_default panels (j, i) 0 = 0
      then Out_channel.printf "."
      else Out_channel.printf "#"
    done;
    Out_channel.printf "\n";
  done;
  return ()
*)

module Tile = struct
  type t =
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball
    [@@deriving equal]

  let of_int = function
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Paddle
    | 4 -> Ball
    | _ -> failwith "invalid tile id"

  let equal x y = [%equal: t] x y
end

let render_output output =
  let rec go output screen =
    match output with
    | [] -> screen
    | x :: y :: tile_id :: tl ->
       go tl (Map.set screen ~key:(x, y) ~data:(Tile.of_int tile_id))
    | _ -> failwith "expected 3 outputs" in
  go output (Map.empty (module Int_pair_comparator))

let _ =
  let program =
    In_channel.input_line_exn stdin
    |> String.strip
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> Array.of_list in
  
  let rendered_output = match expanding_eval program 0 0 [] with
  | Some(Finished(output)) -> render_output output
  | Some(Paused(output, _)) -> render_output output
  | None -> failwith "the program failed" in
  
  let num_block_tiles =
    Map.count
      rendered_output
      ~f:(fun tile -> Tile.equal tile Tile.Block) in

  Out_channel.printf "%d\n" num_block_tiles
