(* Functions to initialise and terminate the R interpreter. *)

external init_r : string array -> int -> int = "init_r" "noalloc"
external terminate : unit -> unit = "end_r" "noalloc"

external init_error_hook : unit -> unit = "r_init_error_hook" "noalloc"

exception Initialisation_failed

let init ?(name    = try Sys.argv.(0) with _ -> "OCaml-R")
         ?(argv    = try List.tl (Array.to_list Sys.argv) with _ -> [])
         ?(env     = Standard.env)
         ?(sigs    = Standard.signal_handlers) () =
  List.iter (function name, value -> Unix.putenv name value) env;
  let r_sigs = match sigs with true -> 0 | false -> 1 in
  match init_r (Array.of_list (name::argv)) r_sigs with
  | 1 -> let () = Callback.register_exception "OCaml-R generic error"
           (Runtime_error ((null_creator ()), "")) in init_error_hook ()
  | _ -> raise Initialisation_failed

module type Interpreter = sig end

module Interpreter (Env : Environment) : Interpreter = struct

  let () = init ~name: Env.name
                ~argv: Env.options
                ~env:  Env.env
                ~sigs: Env.signal_handlers
                ()

  let () = at_exit terminate

end

