open Core

let parse_input_file filename =
  let input = In_channel.read_all filename in
  String.split_lines input