open Base
open Stdio

type opcode =
  | AddOp
  | MulOp
  | LoadOp
  | OutputOp
  | HaltOp

type parameter_mode =
  | PositionMode
  | ImmediateMode

type instruction_header =
  | Add of parameter_mode * parameter_mode * parameter_mode
  | Mul of parameter_mode * parameter_mode * parameter_mode
  | Load of parameter_mode
  | Output of parameter_mode
  | Halt

let opcode_of_int = function
  | 1 -> Some AddOp
  | 2 -> Some MulOp
  | 3 -> Some LoadOp
  | 4 -> Some OutputOp
  | 99 -> Some HaltOp
  | _ -> None

let num_params_of_opcode = function
  | AddOp -> 3
  | MulOp -> 3
  | LoadOp -> 1
  | OutputOp -> 1
  | HaltOp -> 0

let parameter_mode_of_int = function
  | 0 -> Some PositionMode
  | 1 -> Some ImmediateMode
  | _ -> None

let read_param_value program param_mode param =
  match param_mode with
  | PositionMode -> program.(param)
  | ImmediateMode -> param

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
    | [], HaltOp -> Some Halt
    | _ -> None in
  return instruction_header

let rec eval program pc =
  let open Option in
  let* instruction_header = parse_header program.(pc) in
  match instruction_header with
    | Add(m1, m2, _) ->
       let operand1 = read_param_value program m1 program.(pc+1) in
       let operand2 = read_param_value program m2 program.(pc+2) in
       program.(program.(pc+3)) <- operand1 + operand2;
       eval program (pc+4)
    | Mul(m1, m2, _) ->
       let operand1 = read_param_value program m1 program.(pc+1) in
       let operand2 = read_param_value program m2 program.(pc+2) in
       program.(program.(pc+3)) <- operand1 * operand2;
       eval program (pc+4)
    | Load(_) ->
       None (* Load can only ever be the first instruction *)
    | Output(m) ->
       Out_channel.printf "Output: %d\n" (read_param_value program m program.(pc+1));
       eval program (pc+2)
    | Halt -> return ()

let eval0 program pc input =
  let open Option in
  let* instruction_header = parse_header program.(pc) in
  match instruction_header with
    | Load(_) ->
       program.(program.(pc+1)) <- input;
       eval program (pc+2)
    | _ -> None

let _ =
  let program =
    In_channel.input_line_exn stdin
    |> String.strip
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> Array.of_list in
  eval0 program 0 1
