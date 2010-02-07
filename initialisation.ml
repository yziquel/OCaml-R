(* Functions to initialise and terminate the R interpreter. *)

external initialise : string array -> int -> int = "ocamlr_initEmbeddedR" "noalloc"
external terminate : unit -> unit = "ocamlr_endEmbeddedR" "noalloc"

external init_error_hook : unit -> unit = "ocamlr_init_error_hook" "noalloc"

exception Initialisation_failed

let init ?(name     = try Sys.argv.(0) with _ -> "OCaml-R")
         ?(argv     = try List.tl (Array.to_list Sys.argv) with _ -> [])
         ?(env      = Standard.env)
         ?(packages = None)
         ?(sigs     = Standard.signal_handlers) () =
  let env_vars = begin match packages with
    | None -> env
    | Some [] -> ("R_DEFAULT_PACKAGES", "NULL")::env
    | Some libs -> ("R_DEFAULT_PACKAGES", (String.concat ", " libs))::env
    end in
  List.iter (function name, value -> Unix.putenv name value) env_vars;
  let r_sigs = match sigs with true -> 0 | false -> 1 in
  match initialise (Array.of_list (name::argv)) r_sigs with
  | 1 -> let () = Callback.register_exception "OCaml-R generic error"
           (Runtime_error ((null_creator ()), "")) in init_error_hook ()
  | _ -> raise Initialisation_failed

module type Interpreter = sig end

module Interpreter (Env : Environment) : Interpreter = struct

  let () = init ~name: Env.name
                ~argv: Env.options
                ~env:  Env.env
                ~packages: Env.packages
                ~sigs: Env.signal_handlers
                ()

  let () = at_exit terminate

end

