(** {2 Parsing R code.} *)

type parse_status =
(**  Outcome of a parsing request. *)
  | Parse_Null
  | Parse_OK
  | Parse_Incomplete
  | Parse_Error
  | Parse_EOF

exception Parsing_failure of parse_status * string
(**  Exception raised when parsing fails. *)

val parse_string : ?max:int -> string -> lang sxp list
(**  Parse a string of R code into R calls.
  *
  *  @param max If omitted, parse the whole R code, even if
  *  there are multiple statements. Otherwise, maximum
  *  number of statements to parse.
  *  @raise Parsing_failure When parsing fails. *)

val parse : string -> lang sxp
(**  Parse the first R statement in the given R code. *)

