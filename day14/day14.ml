open Base
open Stdio

let div_ru x y = if Int.rem x y = 0 then x/y else x/y + 1

(*
let solve
*)

let () =
  let reactions =
    let lexbuf = Lexing.from_channel stdin in
    Parser.reactions Lexer.read lexbuf in
  (*
  List.iter reactions ~f:(fun (inputs, (n, x)) ->
                               List.iter inputs ~f:(fun (n, x) -> Out_channel.printf "%d %s\n" n x);
                               Out_channel.printf "%d %s\n" n x)
   *)
