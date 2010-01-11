(**  Binding for R Interpreter. It encapsulates the
  *  functionalities of the {b libR.so} shared library provided by the R
  *  software. This enables us to {b embed the R interpreter} into Objective
  *  Caml, to execute R code from Objective Caml and to exchange data
  *  structures between R and Objective Caml.
  *)

(**  {b THREAD SAFETY}
  *
  *  It is important to understand that this binding is a rather low-level
  *  binding of R functionality. As such, it is no more thread-safe than R
  *  itself. Therefore, avoid real threading unless you know what you're
  *  doing...
  *)

(**  {b DATA CONVERSION}
  *
  *  R is an array-oriented language. Therefore, simple values such as
  *  a boolean, a string, a number, are in fact encapsulated, in R, in an
  *  array of booleans, an array of strings, an array of numbers. For a
  *  simple value, the array has only one element.
  *
  *  Moreover, as R is scientific software, it is important that data types
  *  be correctly matched between Objective Caml and R. At the moment, they
  *  are not. I am thinking here of the 31/32 bit issues, or 63/64 bit issue,
  *  or, for instance, of the fact that we are converting R arrays to
  *  Objective Caml lists, which isn't necessarily the smartest choice of
  *  data structures.
  *)

module type Environment =
sig

  val name : string
  (**  This is the name of the first argument of argv for R.
       Mandatory, otherwise libR.so segfaults immediately. *)

  val options : string list
  (**  Other command line options passed to the libR.so library when
    *  initialising R.
    *
    *  See R reference manual, refman.pdf, page 452, section intitled
    *  'Startup - Initialization at Start of an R Session' for details
    *  about the most important command line options.
    *  More options are documented on the following webpage:
    *  http://cran.r-project.org/doc/manuals/R-intro.html#Invoking-R
    *)

  val signal_handlers : bool
  (**  If set to false, asks R not to install its signal handlers. I've
    *  been experiencing weird issues with R signal handlers, since, for
    *  instance, a SIGSEGV is sometimes caught by libR.so, and R asks then
    *  asks whether or not you want to save your workspace, et ceterae. By
    *  default, set to false.
    *)

  val env : (string * string) list
  (**  These are environment variables that needs to be set before
    *  initialising the R interpreter. In the [Standard] module, these
    *  values are determined when the binding itself is compiled.
    *)

end
module Standard : Environment
(**  The [Standard] module contains initialisation details for libR.so.
  *  These informations are determined when the binding is being compiled.
  *)
type sexp
(**  A universal type for wrapped R values. *)

type 'a sxp
(**  A polymorphic type for wrapped R values. 'a represents
  *  the dynamic typing of the underlying R value. *)

type env
(**  Phantom type representing the ENVSXP dynamic R sexptype. *)

type lang
(**  Phantom type representing the LANGSXP dynamic R sexptype. *)

type 'a vec
(**  Polymorphic phantom type representing R array sexptypes. *)

type 'a vecsxp = 'a vec sxp
(**  Type abreviation for R array values. *)

type sexptype =
(**  Algebraic datatype reflecting R's dynamic typing. *)
  | NilSxp
  | SymSxp
  | ListSxp
  | CloSxp
  | EnvSxp
  | PromSxp
  | LangSxp
  | SpecialSxp
  | BuiltinSxp
  | CharSxp
  | LglSxp
  | IntSxp
  | RealSxp
  | CplxSxp
  | StrSxp
  | DotSxp
  | AnySxp
  | VecSxp
  | ExprSxp
  | BcodeSxp
  | ExtptrSxp
  | WeakrefSxp
  | RawSxp
  | S4Sxp
  | FunSxp

val sexptype : sexp -> sexptype
(**  Returns the R dynamic typing of a wrapped R value. *)

type 'a t
(**  This is the type of a wrapped R value whose semantics
  *  is the corresponding 'a type in Objective Caml.
  *)
val symbol : string -> sexp
(**  Retrieves an R symbol from the symbol table, given its name. *)

val bool : bool -> bool vecsxp
(**  Converts an Objective Caml boolean value to an R boolean value,
  *  that is a mono-element array of booleans.
  *) 

val float_list_of_real_vecsxp : sexp -> float list
(**  Converts an R array of real numbers into a list of Objective
  *  Caml floats.
  *)

val strings_of_t : string list t -> string list
(**  Converts an R array of strings into a list of Objective Caml
  *  strings.
  *)

val string : string -> string t
(**  Converts an Objective Caml string to an R string, that is a
  *  mono-element array of strings.
  *)

val strings : string list -> string vecsxp
(**  Converts an Objective Caml list of strings into an R array of
  *  strings.
  *)
val eval_string : string -> sexp
(**  [eval_string] take a string containing R code, and feeds it to the
  *  R interpreter. You get the resulting value back.
  *)

val arg : ('a -> 'b t) -> ?name:string -> 'a -> (string option * sexp) option
(**  Convenience function to wrap up arguments, when mapping R functions
  *  to Objective Caml functions.
  *)

val opt : ('a -> 'b t) -> string -> 'a option -> (string option * sexp) option
(**  Convenience function to wrap up optional arguments, when mapping R functions
  *  to Objective Caml functions.
  *)

val eval : sexp -> (string option * sexp) option list -> sexp
(**  Evaluates an R function given a list of arguments. *)

(**  {b Initialisation}
  *
  *  We provide two mechanisms to activate an R interpreter from OCaml-R:
  *
  *  The first mechanism consists of low-level bindings to the initialisation
  *  and termination functions of the libR.so shared library. While this is
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
  *  them depend on the findlib {b R.interpreter} package, which involves the OCamlR
  *  module. This is also convenient on the toplevel, where you simply have to have
  *  to invoke the {b #require "R.interpreter"} directive to set up the interpreter.
  *)

module type Interpreter = sig

  val loaded: unit

end
(**  Module type of an R interpreter. *)

module Interpreter (Env : Environment) : Interpreter
(**  Functor used to initialise statically an R interpreter, given initialisation
  *  details provided by the provided [Environment] module.
  *)
module Base : sig
module DataFrame :
  sig
    class type t =
      object
        method attribute : string -> sexp
        method classes : string list
        method column : int -> sexp
        method element : int -> int -> sexp
        method names : string list
        method row_names : string list
        method underlying : sexp
      end
    val t_from_R : sexp -> t
  end

end
