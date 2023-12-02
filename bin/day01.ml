open Base
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


let () =
  let input = In_channel.read_all "input.txt" in
  let lines = String.split_lines input in
  printf "%d\n" (part1 (string_list_to_char_list_list lines []))