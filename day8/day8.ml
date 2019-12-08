open Base
open Stdio

let enumerate xs = List.zip_exn (List.range 0 (List.length xs)) xs

let compare_ints_after ~f x y = Int.compare (f x) (f y)

let least_zero_layer layers = 
  layers
  |> enumerate
  |> List.min_elt
       ~compare:(fun (_,l1) (_,l2) ->
         compare_ints_after
           ~f:(List.count ~f:(fun x -> x = 0)) l1 l2)
  |> (fun x -> Option.value_exn x)
  |> snd

let print_pixel pixel = Out_channel.printf "%d" pixel
let print_row row =
  List.iter ~f:print_pixel row;
  Out_channel.printf "\n"

let () =
  let width = 25 in
  let height = 6 in
  let image =
    In_channel.input_line_exn stdin
    |> String.strip
    |> String.to_list
    |> List.map ~f:(fun c -> c |> Char.to_string |> Int.of_string) in
  let layers = List.chunks_of ~length:(width * height) image in

  let lz_layer = least_zero_layer layers in
  let num_ones = List.count ~f:(fun x -> x = 1) lz_layer in
  let num_twos = List.count ~f:(fun x -> x = 2) lz_layer in
  Out_channel.printf "%d\n" (num_ones * num_twos);

  let transposed_layers = List.transpose_exn layers in

  let visible_pixels =
    transposed_layers
    |> List.map
         ~f:(List.fold_left
               ~f:(fun acc x -> if acc = 2 then x else acc)
               ~init:2) in
  let decoded_image = List.chunks_of ~length:width visible_pixels in
  List.iter ~f:print_row decoded_image;

