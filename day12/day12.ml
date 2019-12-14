open Base
open Stdio

module type Vector = sig
  type t
  val zero : t
  val (+) : t -> t -> t
  val compare : t -> t -> t
  val sum : t -> int
  val abs : t -> t
  val equal : t -> t -> bool
end

module Vec3 = struct
  type t =
    { x : int;
      y : int;
      z : int;
    }

  let zero = { x = 0; y = 0; z = 0; }

  let (+) u v =
    { x = u.x + v.x;
      y = u.y + v.y;
      z = u.z + v.z;
    }

  let compare u v =
    { x = compare u.x v.x;
      y = compare u.y v.y;
      z = compare u.z v.z;
    }

  let sum (v: t) = let open Int in v.x + v.y + v.z

  let abs (v: t) : t = { x = abs v.x; y = abs v.y; z = abs v.z; }

  let equal { x; y; z; } { x = x'; y = y'; z = z'; } = x = x' && y = y' && z = z'
end

module Int_vector = struct
  type t = int
  let zero = 0
  let (+) = (+)
  let compare = compare
  let sum x = x
  let abs x = Int.abs x
  let equal x y = x = y
end

module Planet (V : Vector) = struct
  type a = V.t
  type t = { pos : a; vel : a; }

  let make pos vel = { pos; vel; }

  let make_stationary pos = { pos; vel = V.zero; }

  let simulate_step planets =
    let velocities = Array.map planets ~f:(fun {vel; _} -> vel) in
    for i = 0 to Array.length planets - 1 do
      for j = 0 to Array.length planets - 1 do
        velocities.(i) <- V.(velocities.(i) + compare planets.(j).pos planets.(i).pos)
      done
    done;
    Array.mapi
      planets
      ~f:(fun i _ ->
        { pos = V.(planets.(i).pos + velocities.(i));
          vel = velocities.(i); })

    let total_energy { pos; vel; } = V.sum (V.abs pos) * V.sum (V.abs vel)

    let equal p p' = V.equal p.pos p'.pos && V.equal p.vel p'.vel

    let period f ps =
      let rec go ps' n =
        if Array.for_all (Array.zip_exn ps ps') ~f:(fun (p, p') -> equal p p')
        then n
        else go (f ps') (n + 1) in
      go (f ps) 1
end

let rec apply_n n f x =
  if n = 0 then x else apply_n (n-1) f (f x)

let rec gcd a b = if b = 0 then a else gcd b (Int.rem a b)

let lcm a b = a * b / gcd a b

let rec lcm_multi = function
  | [] -> failwith "cannot find lcm of empty list"
  | [x] -> x
  | x :: xs -> lcm x (lcm_multi xs)

let _ =
  let module P1 = Planet(Int_vector) in
  let module P3 = Planet(Vec3) in

  (* input *)
  let planet1 = P3.make_stationary { x = 4; y = 12; z = 13; } in
  let planet2 = P3.make_stationary { x = (-9); y = 14; z = (-3); } in
  let planet3 = P3.make_stationary { x = (-7); y = (-1); z = 2; } in
  let planet4 = P3.make_stationary { x = (-11); y = 17; z = (-1); } in


  let planets = [| planet1; planet2; planet3; planet4 |] in
  let planets' = apply_n 1000 P3.simulate_step planets in
  let energy = Array.sum (module Int) planets' ~f:P3.total_energy in
  Out_channel.printf "Energy: %d\n" energy;

  let xs = Array.map planets ~f:(fun p -> P1.make p.pos.x p.vel.x) in
  let ys = Array.map planets ~f:(fun p -> P1.make p.pos.y p.vel.y) in
  let zs = Array.map planets ~f:(fun p -> P1.make p.pos.z p.vel.z) in
  
  let periods =
    [ P1.period P1.simulate_step xs
    ; P1.period P1.simulate_step ys
    ; P1.period P1.simulate_step zs
    ] in

  let period = lcm_multi periods in
  Out_channel.printf "Period: %d\n" period
