type parse_status =
  | Parse_Null
  | Parse_OK
  | Parse_Incomplete
  | Parse_Error
  | Parse_EOF

exception Parsing_failure of parse_status * string

let parse_status_of_int = function
  | 0 -> Parse_Null
  | 1 -> Parse_OK
  | 2 -> Parse_Incomplete
  | 3 -> Parse_Error
  | 4 -> Parse_EOF
  | _ -> assert false

external raw_parse_string : string -> int -> int * sexp = "r_parse_string"
let parse_string ?max statement =
  let error_code, sexp = raw_parse_string statement
    begin match max with None -> -1 | Some n -> n end in
  match parse_status_of_int error_code with
  | Parse_OK -> sexp
  | _ as status -> raise (Parsing_failure (status, statement))

(* This is wrong!!! but we need it to compile... for now. *)
let parse statement = parse_string ~max:1 statement

