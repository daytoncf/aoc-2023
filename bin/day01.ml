open Core
open Stdio

let explode_string str = Stdlib.List.init (String.length str) (String.get str)

let rec string_list_to_char_list_list lst new_lst =
  match lst with
  | [] -> new_lst
  | str :: rest -> string_list_to_char_list_list rest (explode_string str :: new_lst) 

let rec calculate_calibration_for_line line first last = 
  match line with
  | [] -> if last <> 0 
    then (first * 10 + last)
    else (first * 10 + first)
  | '0' .. '9' as ch :: tail -> if first = 0 
    then calculate_calibration_for_line tail ((Char.to_int ch) - 48) 0 
    else calculate_calibration_for_line tail first ((Char.to_int ch) - 48)
  | _ :: tail -> calculate_calibration_for_line tail first last

let part1 lines =
  let rec part1' lines sum = 
    match lines with
    | [] -> sum
    | line :: lines -> part1' lines ((calculate_calibration_for_line line 0 0) + sum)
  in part1' lines 0

type case = string * int

let cases_map : case list = [
  ("one", 1);
  ("two", 2);
  ("three", 3);
  ("four", 4);
  ("five", 5);
  ("six", 6);
  ("seven", 7);
  ("eight", 8);
  ("nine", 9);
  ("1", 1);
  ("2", 2);
  ("3", 3);
  ("4", 4);
  ("5", 5);
  ("6", 6);
  ("7", 7);
  ("8", 8);
  ("9", 9);
]

let find_first str =
  List.range 0 (String.length str)
  |> List.find_map ~f:(fun pos ->
      List.find_map cases_map ~f:(fun (search_pattern, numeric_value) ->
        match String.substr_index ~pos ~pattern:search_pattern str with
        | Some matched when matched = pos -> Some numeric_value
        | _ -> None  
      )
    ) |> Option.value_exn

let find_last str =
  List.range ~stride:(-1) (String.length str) (-1) 
  |> List.find_map ~f:(fun pos ->
      List.find_map cases_map ~f:(fun (search_pattern, numeric_value) ->
        match String.substr_index ~pos ~pattern:search_pattern str with
        | Some _ -> Some numeric_value
        | _ -> None
      )
    ) |> Option.value_exn

let calculate_calibration line =
  ((find_first line) * 10) + (find_last line)

let part2 lines =
  List.fold lines ~init:0 ~f:(fun acc line -> acc + calculate_calibration line)

let solve_day1_part1 =
  let input = In_channel.read_all "input.txt" in
  let lines = String.split_lines input in
  printf "%d\n" (part1 (string_list_to_char_list_list lines []))

let solve_day1_part2 =
  let input = In_channel.read_all "input.txt" in
  let lines = String.split_lines input in
  printf "%d\n" (part2 lines)

let () = 
  solve_day1_part1; solve_day1_part2