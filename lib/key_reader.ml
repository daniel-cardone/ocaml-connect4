open Unix

type key =
| Arrow_left
| Arrow_right
| Arrow_up
| Arrow_down
| Enter
| Space
| Char of char
| Quit
| Unknown of string

type terminal_state = {
  fd : file_descr;
  original_attrs : terminal_io;
}

let setup_raw_mode fd =
  let original_attrs = tcgetattr fd in
  let raw_attrs = { original_attrs with
    c_icanon = false;
    c_echo = false;
    c_vmin = 1;
    c_vtime = 0;
  } in
  tcsetattr fd TCSADRAIN raw_attrs;
  { fd; original_attrs }

let restore_terminal { fd; original_attrs } =
  tcsetattr fd TCSADRAIN original_attrs

let parse_escape_sequence buf len =
  match Bytes.sub_string buf 0 len with
  | "\027[A" -> Arrow_up
  | "\027[B" -> Arrow_down
  | "\027[C" -> Arrow_right
  | "\027[D" -> Arrow_left
  | s -> Unknown s

let read_char fd =
  let buf = Bytes.create 1 in
  match read fd buf 0 1 with
  | 0 -> None
  | _ -> Some (Bytes.get buf 0)
  | exception _ -> None

let read_escape_sequence fd =
  let buf = Bytes.create 3 in
  Bytes.set buf 0 '\027';
  match read fd buf 1 2 with
  | n when n > 0 -> parse_escape_sequence buf (1 + n)
  | _ -> Unknown "ESC"
  | exception _ -> Unknown "ESC"

let parse_key = function
| '\027' -> fun fd -> read_escape_sequence fd
| '\n' | '\r' -> fun _ -> Enter
| ' ' -> fun _ -> Space
| c -> fun _ -> Char c

let read_key_with_state state =
  Stdlib.flush Stdlib.stdout;
  match read_char state.fd with
  | None -> Quit
  | Some c -> parse_key c state.fd

let with_raw_mode f =
  let state = setup_raw_mode stdin in
  let sigint_handler = Sys.signal Sys.sigint (Sys.Signal_handle (fun _ ->
    restore_terminal state;
    exit 130
  )) in
  Fun.protect 
    ~finally:(fun () -> 
      restore_terminal state;
      Sys.set_signal Sys.sigint sigint_handler)
    (fun () -> f state)

let read_key () =
  with_raw_mode read_key_with_state
