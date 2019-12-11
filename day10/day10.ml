open Base
open Stdio

let rec gcd n m = if m = 0 then n else gcd m (Int.rem n m)

let is_cohalflinear (x1, y1) (x2, y2) =
  (* If they both lie on the x axis, do their y's have the same sign? *)
  if x1 = 0 && x2 = 0 then y1 * y2 >= 0
  (* If they both lie on the y axis, do their y's have the same sign? *)
  else if y1 = 0 && y2 = 0 then x1 * x2 >= 0
  (* If they are in the same quadrant, is x1 / x2 = y1 / y2 ? *)
  else if x1 * x2 > 0 && y1 * y2 > 0 then x1 * y2 = x2 * y1
  else false

(* pre: (x1, y1) and (x2, y2) are cohalflinear *)
let is_closer (x1, y1) (x2, y2) =
  abs x1 < abs x2 || abs y1 < abs y2
  
let positions_of_map (map : char list list) : (int * int) array =
  let positions = ref [] in
  List.iteri map ~f:(fun i cs ->
      List.iteri cs ~f:(fun j c ->
          if Char.equal c '#' then positions := (i, j) :: !positions));
  Array.of_list !positions

let () =
  let map =
    In_channel.input_lines stdin
    |> List.map ~f:String.to_list in
  let asteroids = positions_of_map map in
  let counts = Array.create 0 ~len:(Array.length asteroids) in
  Array.iteri asteroids
    ~f:(fun i (xo, yo) ->
      Array.iter asteroids 
       ~f:(fun (x1, y1) ->
         if xo = x1 && yo = y1 then ()
         else
           let (x1', y1') = (x1 - xo, y1 - yo) in
           let visible =
             Array.exists asteroids
               ~f:(fun (x2, y2) ->
                 if xo = x2 && yo = y2 then false
                 else
                   let (x2', y2') = (x2 - xo, y2 - yo) in
                   is_cohalflinear (x1', y1') (x2', y2')
                   && is_closer (x2', y2') (x1', y1'))
             |> not in
           if visible then counts.(i) <- counts.(i) + 1));
  match Array.max_elt counts ~compare:Int.compare with
  | Some(n) -> Out_channel.printf "%d\n" n
  | None -> ()
