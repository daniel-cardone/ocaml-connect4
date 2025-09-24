open ANSITerminal
open Key_reader

let print_options options =
  print_string [Reset] "\n\t";
  List.iter (fun (styles, text) -> print_string styles (text ^ "\t\t")) options;
  print_string [Reset] "\n"

let get_choice ?(cursor_style=[red]) spacer options option_lengths =
  let len = List.length options in
  let rec aux curr =
    restore_cursor ();
    erase Eol;
    print_string cursor_style (spacer curr ^ String.make option_lengths.(curr) '^');
    let (curr', stop) =
      match read_key () with
      | Arrow_left -> (if curr <= 0 then 0 else curr - 1), false
      | Arrow_right -> (if curr >= len - 1 then len - 1 else curr + 1), false
      | Enter | Space -> curr, true
      | _ -> curr, false
    in
    if stop then snd (List.nth options curr') else aux curr'
  in aux 0

let prompt ?(question_pos=(1, 2)) options message =
  let option_lengths = Array.of_list (List.map (fun (_, text) -> String.length text) options) in
  let rec gen_spacing acc = function
  | 0 -> acc
  | n -> gen_spacing (acc ^ String.make option_lengths.(n - 1) ' ' ^ "\t\t") (n - 1)
  in
  set_cursor (fst question_pos) (snd question_pos);
  erase Below;
  print_string [yellow] (message ^ "\n");
  print_options options;
  set_cursor (fst question_pos) (snd question_pos + 3);
  save_cursor ();
  get_choice (gen_spacing "\t") options option_lengths
