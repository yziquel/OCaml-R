(**  {2 Initialisation.}
  *
  *  We provide two mechanisms to activate an R interpreter from OCaml-R:
  *
  *  The first mechanism consists of low-level bindings to the initialisation
  *  and termination functions of the [libR.so] shared library. While this is
  *  a rather flexible approach, it has the downside of not being a very static
  *  approach, specifically if your intention if to write Objective Caml bindings
  *  for a dependent bunch of R packages.
  *
  *  The second mechanism is a static, functorial approach: You just have to
  *  create a module with the [Interpreter] functor to initialise the R interpreter.
  *  You provide initialisation details through a module of module type [Environment],
  *  and [Interpreter] will set it up correctly.
  *
  *  This functorial facility is available from the OCamlR module: This OCamlR module
  *  has the sole purpose of initialising the R interpreter with the [Standard]
  *  [Environment] module. No need to worry about initialisation details.
  *
  *  To create bindings for a dependent bunch of R packages, you simply have to make
  *  them depend on the findlib [R.interpreter] package, which involves the OCamlR
  *  module. This is also convenient on the toplevel, where you simply have to have
  *  to invoke the [#require "R.interpreter"] directive to set up the interpreter.
  *)

exception Initialisation_failed
(**  Denotes failure to initialise the R interpreter. *)

val init : ?name:string -> ?argv:string list -> ?env:(string * string) list -> ?sigs:bool -> unit -> unit
(**  [init] initialises the embedded R interpreter.
  *
  *  @param name Name of program. Defaults to Sys.argv.(0).
  *  @param argv Command line options given to [libR.so]. Defaults to rest of Sys.argv.
  *  @param env Environment variables to be set for R. Defaults to reasonable values.
  *  @param sigs If [false], stops R from setting his signal handlers. Defaults to [false].
  *
  *  @raise Initialisation_failed *)

val terminate : unit -> unit
(**  Terminates the R session. *)

module type Interpreter = sig end
(**  Module type of an R interpreter. *)

module Interpreter (Env : Environment) : Interpreter
(**  Functor used to initialise statically an R interpreter, given initialisation
  *  details provided by the provided [Env] module.
  *)

