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

let visible_from (xo, yo) asteroids (x1, y1)  =
  if xo = x1 && yo = y1 then false
  else
    let (x1', y1') = (x1 - xo, y1 - yo) in
    Array.exists asteroids
      ~f:(fun (x2, y2) ->
        if xo = x2 && yo = y2 then false
        else
          let (x2', y2') = (x2 - xo, y2 - yo) in
          is_cohalflinear (x1', y1') (x2', y2')
          && is_closer (x2', y2') (x1', y1'))
    |> not

let angle_of_coords (x, y) =
  let x' = Float.of_int x in
  let y' = Float.of_int (-y) in
  let open Float in
  if x' >= 0. && y' > 0. then Float.atan (x' /. y')
  else if x' > 0. && y' <= 0. then Float.pi /. 2. +. Float.atan (Float.abs y' / Float.abs x')
  else if x' <= 0. && y' < 0. then Float.pi +. Float.atan (Float.abs x' / Float.abs y')
  else 3. *. Float.pi /. 2. +. Float.atan (Float.abs y' / Float.abs x')

let compare_angle (x1, y1) (x2, y2) =
  Float.compare (angle_of_coords (x1, y1)) (angle_of_coords (x2, y2))
  
let positions_of_map (map : char list list) : (int * int) array =
  let positions = ref [] in
  List.iteri map ~f:(fun j cs ->
      List.iteri cs ~f:(fun i c ->
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
  let enumerated_counts =
    Array.zip_exn
      (Array.init (Array.length counts) ~f:(fun i -> i))
      counts in
  let compare_tagged (_,x) (_,y) = Int.compare x y in
  let _, (xo, yo) =
    match Array.max_elt enumerated_counts ~compare:compare_tagged with
    | Some(i, _) -> (i, asteroids.(i))
    | None -> failwith "cannot find maximum of array" in
  let (visible, _) =
    Array.partition_tf asteroids ~f:(visible_from (xo, yo) asteroids) in
  assert (Array.length visible >= 200);
  let visible_relative = Array.map visible ~f:(fun (x, y) -> (x - xo, y - yo)) in
  let sorted = Array.sorted_copy visible_relative ~compare:compare_angle in
  let (x200, y200) = sorted.(199) in
  Out_channel.printf "%d %d\n" (x200 + xo) (y200 + yo);
