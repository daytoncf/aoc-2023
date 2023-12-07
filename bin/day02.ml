open Core

(* let is_game_valid str =
  let draws = String.split ~on:';' str in

let is_hand_valid str = 
  let cube_sets = String.split ~on:',' str
  |> List.map ~f:(fun substr ->
      let trimmed = String.strip substr
    ) *)

type subset = { count : int; color : string; }

let parse_subset str =
  let trimmed = String.strip str in
  List.find_map ["red"; "green"; "blue";] ~f:(fun color ->
    match String.is_substring trimmed ~substring:color with
    | false -> None
    | true -> Some { count = (String.get trimmed 0 |> Char.to_int) - 48 ; color = color }
  )
  
let is_subset_valid subset = 
  match subset with
  | { color = "red"; count; } -> count <= 12
  | { color = "green"; count; } -> count <= 13
  | { color = "blue"; count; } -> count <= 14
  | _ -> print_endline "Invalid subset"; false

let () = 
  print_endline "test"