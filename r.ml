module type Environment =
sig

  val name : string    (* This is the first argument of argv for R.
                          Mandatory, otherwise libR.so segfaults. *)

  (** See R reference manual, refman.pdf, page 452, section intitled
      'Startup - Initialization at Start of an R Session' for details
      about the most important command line options.
      More options are documented on the following webpage:
      http://cran.r-project.org/doc/manuals/R-intro.html#Invoking-R *)

  val options : string list

  (* signal_handlers, if set to false, asks R not to install its
     signal handlers. I've been experiencing weird issues with R signal
     handlers, since, for instance, a SIGSEGV originating from OCaml is
     caught by libR.so, and R asks then asks whether or not you want to
     save your workspace, et ceterae. By default, set to false. *)
  val signal_handlers : bool

  val env : (string * string) list

end

module Standard : Environment = struct

  let name="OCaml-R"
  let options = ["--vanilla"; "--slave"]
  let signal_handlers = false
  let env = [
    "R_ARCH", "";
    "R_BROWSER", "xdg-open";
    "R_BZIPCMD", "/bin/bzip2";
    "R_DOC_DIR", "/usr/share/R/doc";
    "R_DVIPSCMD", "/usr/bin/dvips";
    "R_GZIPCMD", "/bin/gzip";
    "R_HOME", "/usr/lib64/R";
    "R_INCLUDE_DIR", "/usr/share/R/include";
    "R_LATEXCMD", "/usr/bin/latex";
    "R_LIBS_SITE", "/usr/local/lib/R/site-library:/usr/lib/R/site-library:/usr/lib/R/library";
    "R_LIBS_USER", "~/R/x86_64-pc-linux-gnu-library/2.10";
    "R_MAKEINDEXCMD", "/usr/bin/makeindex";
    "R_PAPERSIZE", "letter";
    "R_PAPERSIZE_USER", "a4";
    "R_PDFLATEXCMD", "/usr/bin/pdflatex";
    "R_PDFVIEWER", "/usr/bin/xdg-open";
    "R_PLATFORM", "x86_64-pc-linux-gnu";
    "R_PRINTCMD", "/usr/bin/lpr";
    "R_RD4DVI", "ae";
    "R_RD4PDF", "times,hyper";
    "R_SHARE_DIR", "/usr/share/R/share";
    "R_TEXI2DVICMD", "/usr/bin/texi2dvi";
    "R_UNZIPCMD", "/usr/bin/unzip";
    "R_ZIPCMD", "/usr/bin/zip";
  ]

end
(* Types of wrapped R SEXP values. sxp is a polymorphic type
   wrapping up the monomorphic type sexp *)
type sexp

(* Argument types for the polymorphic 'a sxp type. *)
type 'a sxp = sexp
type nil                         (* For NILSXP *)
type sym                      (* For SYMSXP *)
type 'a lisplist                 (* For LISTSXP, and LANGSXP *)
type simple                      (* For LISTSXP *)
type pairlist = simple lisplist  (* For LISTSXP *)
type clos                        (* For CLOSXP *)
type env                         (* For ENVSXP *)
type prom                        (* For PROMSXP *)
type call                        (* For LANGSXP *)
type lang = call lisplist        (* For LANGSXP *)
type builtin                     (* For BUILTINSXP *)
(* Phantom type vec, and phantom subtype vecsxp. *)
type 'a vec                      (* For all the VECSXPs *)
type 'a vecsxp = 'a vec sxp
type vec_char = char vec         (* For CHARSXP *)
type vec_lgl  = bool vec         (* For LGLSXP *)
type vec_int  = int  vec         (* For INTSXP *)
    (* Or shouldn't it be int32 vec ? *)
type vec_real = float vec        (* For REALSXP *)
type vec_str  = string vec       (* For STRSXP *)
type vec_sexp = sexp vec

(* Algebraic type reflecting R's dynamic typing. *)
type sexptype =
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

external sexptype_of_sexp : sexp -> int = "r_sexptype_of_sexp" "noalloc"
let sexptype s = match (sexptype_of_sexp s) with
  | 0  -> NilSxp
  | 1  -> SymSxp
  | 2  -> ListSxp
  | 3  -> CloSxp
  | 4  -> EnvSxp
  | 5  -> PromSxp
  | 6  -> LangSxp
  | 7  -> SpecialSxp
  | 8  -> BuiltinSxp
  | 9  -> CharSxp
  | 10 -> LglSxp
    (* Integer range is not defined here. *)
  | 13 -> IntSxp
  | 14 -> RealSxp
  | 15 -> CplxSxp
  | 16 -> StrSxp
  | 17 -> DotSxp
  | 18 -> AnySxp
  | 19 -> VecSxp
  | 20 -> ExprSxp
  | 21 -> BcodeSxp
  | 22 -> ExtptrSxp
  | 23 -> WeakrefSxp
  | 24 -> RawSxp
  | 25 -> S4Sxp
  (* 99 represents a 'dummy' type for functions, with is an
     umbrella for Closure, Builtin or Special types. *)
  | 99 -> FunSxp
  | _ -> failwith "R value with type not specified in Rinternals.h"

let string_of_sexptype = function
  | NilSxp     -> "NilSxp"
  | SymSxp     -> "SymSxp"
  | ListSxp    -> "ListSxp"
  | CloSxp     -> "CloSxp"
  | EnvSxp     -> "EnvSxp"
  | PromSxp    -> "PromSxp"
  | LangSxp    -> "LangSxp"
  | SpecialSxp -> "SpecialSxp"
  | BuiltinSxp -> "BuiltinSxp"
  | CharSxp    -> "CharSxp"
  | LglSxp     -> "LglSxp"
  | IntSxp     -> "IntSxp"
  | RealSxp    -> "RealSxp"
  | CplxSxp    -> "CplxSxp"
  | StrSxp     -> "StrSxp"
  | DotSxp     -> "DotSxp"
  | AnySxp     -> "AnySxp"
  | VecSxp     -> "VecSxp"
  | ExprSxp    -> "ExprSxp"
  | BcodeSxp   -> "BcodeSxp"
  | ExtptrSxp  -> "ExtptrSxp"
  | WeakrefSxp -> "WeakrefSxp"
  | RawSxp     -> "RawSxp"
  | S4Sxp      -> "S4Sxp"
  | FunSxp     -> "FunSxp"

let is_function x = match sexptype x with
  | CloSxp | SpecialSxp | BuiltinSxp | FunSxp -> true
  | _ -> false
external sexp_equality : sexp -> sexp -> bool = "r_sexp_equality"

(* R constants - global symbols in libR.so. *)
(* We are looking for a clean solution
   for the typing of the R NULL. What should it be
   in OCaml? An 'a option mapping to None? *)
external null_creator : unit -> nil sxp = "r_null"
external dots_symbol_creator : unit -> sexp = "r_dots_symbol"
external missing_arg_creator : unit -> sexp = "r_missing_arg"
external base_env_creator : unit -> sexp = "r_base_env"

(* R_GlobalEnv is not a constant, but rather a constant pointer,
   that gets updated by R itself. *)
external global_env : unit -> sexp = "r_global_env"type 'a t = sexp
type 'a promise = 'a Lazy.t t
(* Memory allocation is done via functions exported from memory.c. *)


(**  Allocates a pairlist.
  *
  *  r_alloc_list takes an integer i as argument, and returns an R
  *  pairlist, also called a LISTSXP, of size i.
  *
  *  Such a function is necessary in order to avoid the tail
  *  recursion issue. While doing 'let x = f () in x::(aux y)' is
  *  an acceptable way to make aux tail recurse in Objective Caml,
  *  this relies on the way Objective Caml constructs list with
  *  the :: constructor. This feature is not available when we use
  *  stub code around the CONS macro for R. Allocating a list and
  *  filling it in imperatively is a workaround.
  *)

external alloc_list : int -> 'a lisplist sxp = "r_alloc_list"


(**  Allocates a logical vector.
  *
  *  alloc_lgl_vector takes an integer i as argument, and returns
  *  an R vector of logical values of size i.
  *)

external alloc_lgl_vector : int -> vec_lgl sxp = "r_alloc_lgl_vector"


(**  Allocates a vector of integers.
  *
  *  alloc_int_vector takes an integer i as argument, and returns
  *  an R vector of integer values of size i.
  *)

external alloc_int_vector : int -> vec_int sxp = "r_alloc_int_vector"


(**  Allocates a vector of strings.
  *
  *  r_alloc_str_vector takes an integer i as argument, and returns
  *  an R vector of i strings.
  *)

external alloc_str_vector : int -> vec_str sxp = "r_alloc_str_vector"

(* Low-level data manipulation functions. *)

(* What follows is low-level accessor functions, in order to inspect
   in details the contents of SEXPs and VECSEXPs. *)


(**  Returns the attributes of a SEXP or VECSEXP
  *
  *  inspect_attributes takes a SEXP or a VECSEXP as
  *  arguments and returns its attributes, as a SEXP.
  *)

external inspect_attributes : sexp -> sexp = "inspect_attributes"


(**  Returns the length of a VECSEXP.
  *
  *  length_of_vecsxp takes a VECSEXP as argument and
  *  returns its length, i.e., the number of elements.
  *)

external length_of_vecsxp : 'a vecsxp -> int = "inspect_vecsxp_length"


(**  Returns the offset at the primitive can be found.
  *
  *  inspect_primsxp_offset take a SEXP denoting a function primitive
  *  as argument, and returns its offset in the table of primitives.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  *)

external inspect_primsxp_offset  : builtin sxp -> int = "inspect_primsxp_offset"


(**  Returns the name of a symbol.
  *
  *  inspect_symsxp_pname take a SYMSXP as argument, and returns the
  *  SEXP containing its name.
  *)

external inspect_symsxp_pname    : sym sxp -> sexp = "inspect_symsxp_pname"


(**  Returns the value of a symbol.
  *
  *  inspect_symsxp_value takes a SYMXSP as argument, and returns the
  *  SEXP containing its value.
  *)

external inspect_symsxp_value    : sym sxp -> sexp = "inspect_symsxp_value"


(**  Returns the internal sexp of a symbol.
  *
  *  inspect_symsxp_internal takes a SYMSXP as argument, and returns the
  *  SEXP containing its internal value.
  *)

external inspect_symsxp_internal : sym sxp -> sexp = "inspect_symsxp_internal"


(**  Returns the head element of a pairlist.
  *
  *  inspect_listsxp_carval takes a pairlist as argument, and
  *  returns the SEXP containing its head element.
  *)

external inspect_listsxp_carval  : 'a lisplist sxp -> sexp = "inspect_listsxp_carval"


(**  Returns the tail pairlist of a pairlist.
  *
  *  inspect_listsxp_cdrval takes a pairlist as argument,
  *  and returns its tail pairlist.
  *)

external inspect_listsxp_cdrval  : 'a lisplist sxp -> sexp = "inspect_listsxp_cdrval"


(**  Returns the tag value of the head element of a pairlist.
  *
  *  inspect_listsxp_tagval takes a pairlist as argument, and
  *  returns the tag value of the head element of the pairlist.
  *)

external inspect_listsxp_tagval  : 'a lisplist sxp -> sexp = "inspect_listsxp_tagval"


(**  Returns the frame of an evironment.
  *
  *  inspect_envsxp_frame takes an environment as argument,
  *  and returns the frame of this environment.
  *)

external inspect_envsxp_frame    : env sxp -> sexp = "inspect_envsxp_frame"


(**  Returns the enclosing environment of an environment.
  *
  *  inspect_envsxp_enclos takes an environment as argument,
  *  and returns its enclosing environment.
  *)

external inspect_envsxp_enclos   : env sxp -> sexp = "inspect_envsxp_enclos"


(**  Returns the hash table of an environment.
  *
  *  inspect_envsxp_hashtabl takes an environment as argument,
  *  and returns its hash table.
  *)

external inspect_envsxp_hashtab  : env sxp -> sexp = "inspect_envsxp_hashtab"


(**  Returns the list of formal arguments of a closure.
  *
  *  inspect_closxp_formals takes a closure as argument,
  *  and returns the list of its formal arguments.
  *)

external inspect_closxp_formals  : clos sxp -> sexp = "inspect_closxp_formals"


(**  Returns the body of a closure.
  *
  *  inspect_closxp_body takes a closure as argument,
  *  and returns the body of this closure.
  *)

external inspect_closxp_body     : clos sxp -> sexp = "inspect_closxp_body"


(**  Returns the environment of a closure.
  *
  *  inspect_closxp_env takes a closure as argument,
  *  and returns the environment of this closure.
  *)

external inspect_closxp_env      : clos sxp -> sexp = "inspect_closxp_env"


(**  Returns the value of a promise.
  *
  *  inspect_promsxp_value takes a promise as argument,
  *  and returns the value of this promise.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  *)

external inspect_promsxp_value   : prom sxp -> sexp = "inspect_promsxp_value"


(**  Returns the expression of a promise.
  *
  *  inspect_promsxp_expr takes a promise as argument,
  *  and returns the value of this promise.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  *)

external inspect_promsxp_expr    : prom sxp -> sexp = "inspect_promsxp_expr"


(**  Returns the environment of a promise.
  *
  *  inspect_promsxp_env takes a promise as argument,
  *  and returns the environment of this promise.
  *
  *  Note: This function bypasses the provided API, and requires the
  *  use of the #define USE_RINTERNALS directive.
  *)

external inspect_promsxp_env     : prom sxp -> sexp = "inspect_promsxp_env"


(**  Returns an element of a logical vector.
  *
  *  access_lgl_vecsxp takes a logical vector as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_lgl_vecsxp  : vec_lgl  sxp -> int -> bool         = "r_access_lgl_vecsxp"


(**  Returns an element of a vector of integers.
  *
  *  access_int_vecsxp takes a vector of integers as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_int_vecsxp  : vec_int  sxp -> int -> int          = "r_access_int_vecsxp"


(**  Returns an element of a vector of real numbers.
  *
  *  access_real_vecsxp takes a vector of integers as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_real_vecsxp : vec_real sxp -> int -> float        = "r_access_real_vecsxp"


(**  Returns an element of a vector of strings.
  *
  *  access_str_vecsxp takes a vector of strings as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_str_vecsxp  : vec_str  sxp -> int -> string       = "r_access_str_vecsxp"


(**  Returns an element of a vector of SEXPs.
  *
  *  access_sexp_vecsxp takes a vector of SEXPs as argument,
  *  and an offset, and returns the element at this offset.
  *)

external access_sexp_vecsxp : vec_sexp sxp -> int -> sexp         = "r_access_sexp_vecsxp"
external write_listsxp_carval : 'a lisplist sxp -> sexp -> unit = "r_write_lisplist_carval"
external write_listsxp_tagval : 'a lisplist sxp -> sexp -> unit = "r_write_lisplist_tagval"

let write_listsxp_element l tag elmnt =
  let () = write_listsxp_tagval l tag in
  let () = write_listsxp_carval l elmnt in
  ()

(**  Sets the element of a logical vector.
  *
  *  assign_lgl_vecsxp takes a logical vector as first argument,
  *  an offset as second argument, and a boolean as third argument,
  *  and sets the vector's offset element to the boolean's value.
  *)

external assign_lgl_vecsxp  : vec_lgl  sxp -> int -> bool -> unit = "r_assign_lgl_vecsxp"


(**  Sets the element of a vector of integers.
  *
  *  assign_int_vecsxp takes a vector of integers as first argument,
  *  an offset as second argument, and an integer as third argument,
  *  and sets the vector's offset element to the integer's value.
  *
  *  Question: should we rather map R's integers to int32s?
  *)

external assign_int_vecsxp  : vec_int  sxp -> int -> int -> unit = "r_assign_int_vecsxp"


(**  Sets the element of a vector of string.
  *
  *  assign_str_vecsxp takes a vector of strings as first argument,
  *  an offset as second argument, and a string as third argument,
  *  and sets the vector's offset element to the string's value.
  *)

external assign_str_vecsxp  : vec_str  sxp -> int -> string -> unit = "r_assign_str_vecsxp"
external force_promsxp : prom sxp -> sexp = "r_eval_sxp"

let force : 'a promise -> 'a t = force_promsxp

(* For lazy evaluation, we have an issue here: R promises
   are recursively forced by eval. This means that the
   OCaml type system would be broken, because we would need
   to have 'a Lazy.t R.t = 'a Lazy.t Lazy.t R.t. There's two
   solutions:

   -1- 'a R.t would denote a R value of type 'a, lazy or not.
       This is suboptimal, because the OCaml type system could
       and should express these lazy semantics.

   -2- Make a dynamic check on the nature of the argument of
       the force function. If it is a lazy lazy value, we
       should force it manually, with OCaml semantics. If not,
       we can run eval on it. *)(* There's a lot of stuff concerning symbols and environments in the
   envir.c file of the R source code. *)

external install : string -> sym sxp = "r_install"

external findvar : sym sxp -> prom sxp = "r_findvar"

external findfun : sym sxp -> prom sxp = "r_findfun"

let symbol : string -> sexp = fun s ->
  let var = force (findvar (install s)) in

  (* If we try to retrieve a function, we should use findfun. If we
     use findvar, we indeed get a closure, but a closure for a generic
     function in the sense of R objects:

     CLOSURE {formals = LIST [(ARG "object", PLACE); (ARG "...", PLACE)];

     and you get runtime errors. This is why we have a dynamic type check
     here, and this is why this symbol should be used as little as
     possible at R runtime. *)

  match is_function var with
  | false -> var
  | true -> force (findfun (install s))
let rec list_of_lisplist (ll : 'a lisplist sxp) =
  match sexptype ll with
  | NilSxp -> []   (* Typing will have to take into account that NULL is a list. *)
  | ListSxp | LangSxp | DotSxp ->  (* There's also a typing issue here... *)
  ( (inspect_listsxp_tagval ll), (inspect_listsxp_carval ll))
  :: (list_of_lisplist (inspect_listsxp_cdrval ll))
  | _ -> failwith "Conversion failure in list_of_lisplist."

let lisplist_of_list (l: (sexp * sexp) list) =
  let r_l = alloc_list (List.length l) in
  let cursor = ref r_l in List.iter
  begin function (tag, value) ->
    let () = write_listsxp_element !cursor tag value in
    cursor := inspect_listsxp_cdrval !cursor
  end l; r_l

external cons : sexp -> sexp -> sexp = "r_cons"
external tag : sexp -> string -> unit = "r_tag"
external set_langsxp : sexp -> unit = "r_set_langsxp"

let langsxp (f: sexp) (args: (string option * sexp) list) : lang sxp =
  let lcons hd tl = let x = cons hd tl in set_langsxp x; x in
  lcons f begin List.fold_right begin fun (t, hd) tl ->
    let x = cons hd tl in match t with
    | None -> x | Some name -> tag x name; x
  end args (null_creator ()) end

external string_of_charsxp : vec_char sxp -> string = "r_internal_string_of_charsxp"

let list_of_vecsxp (access: 'a vecsxp -> int -> 'a) (s: 'a vecsxp) : 'a list =
  let lngth = length_of_vecsxp s in
  let rec aux n s = match n with | 0 -> [] | _ ->
    let x = access s (lngth - n) in x::(aux (n - 1) s)
  in aux lngth s

let vecsxp_of_list (alloc : int -> 'a vecsxp) (assign : 'a vecsxp -> int -> 'a -> unit) (l: 'a list) : 'a vecsxp =
  let s = alloc (List.length l) in
  let rec aux offset = function | [] -> () | hd::tl ->
    let () = assign s offset hd in aux (1 + offset) tl
  in aux 0 l; s

let bool_list_of_lgl_vecsxp   = list_of_vecsxp access_lgl_vecsxp
let lgl_vecsxp_of_bool_list   = vecsxp_of_list alloc_lgl_vector assign_lgl_vecsxp
let bool b = lgl_vecsxp_of_bool_list [b]

let int_list_of_int_vecsxp    = list_of_vecsxp access_int_vecsxp
let int_vecsxp_of_int_list    = vecsxp_of_list alloc_int_vector assign_int_vecsxp
let int i = int_vecsxp_of_int_list [i]

let float_list_of_real_vecsxp = list_of_vecsxp access_real_vecsxp

let string_list_of_str_vecsxp = list_of_vecsxp access_str_vecsxp
let str_vecsxp_of_string_list = vecsxp_of_list alloc_str_vector assign_str_vecsxp
let strings_of_t : string list t -> string list = string_list_of_str_vecsxp
let string_of_t : string t -> string = fun t -> access_str_vecsxp t 0
  (* We access only the first element, because static typing is supposed to
     ensure that the str vecsxp contains only one element. *)
external string : string -> string t = "r_strsxp_of_string"
let strings = str_vecsxp_of_string_list

let sexp_list_of_sexp_vecsxp = list_of_vecsxp access_sexp_vecsxp
(**  Get the S3 class of a given SEXP.
  *
  *  r_s3_class takes a SEXP as argument, and returns the S3 class
  *  attribute of the given SEXP.
  *)

external s3_class : sexp -> sexp = "r_s3_class"

external aux_get_attrib : sexp -> sexp -> sexp = "r_get_attrib"

let get_attrib s name = aux_get_attrib s (install name)

module S3 = struct

  class type t = object

    val underlying    : sexp
    method underlying : sexp
    method attribute  : string -> sexp
    method classes    : string list

  end

  class from_R r : t = object

    val underlying = r
    method underlying = underlying
    method attribute s = get_attrib underlying s
    method classes = strings_of_t (get_attrib underlying "class")

  end

  let t_from_R r : t = new from_R r

end

module type Types = sig

  type t
  val recursive : t Lazy.t -> t
  val build : (sexp -> t) -> sexp -> t

end

module Parsing (M : Types) = struct

  (* General parsing function for internal R structures, i.e. SEXPs. *)

  let t_of_sexp (s : sexp) =
    let rec aux sexps_seen s =
      let is_found (ss, _) = sexp_equality s ss in
      begin match (try Some (List.find is_found sexps_seen) with _ -> None) with
      | None -> let rec x = lazy (M.build (aux ((s, x)::sexps_seen)) s) in Lazy.force x
      | Some (_, t_lazy) -> M.recursive t_lazy
      end
    in aux [] s

end

module CTypes = struct

  (* Type definitions. *)

  type t = | Recursive of t Lazy.t | Val of t_val

  and t_val = {
    (* sxpinfo : sxpinfo;   *)
    (* attrib  : t;         *)
    (* gengc_nextnode : t;  *)
    (* gengc_prevnode : t;  *)
    content : t_content
  }

  (* and sxpinfo = {
    type  : sexptype;
    obj   : int;
    named : int;
    gp    : int;
    mark  : int;
    debug : int;
    trace : int;
    spare : int;
    gcgen : int;
    gccls : int;
  }*)

  and t_content =
    | NILSXP
    | SYMSXP of sxp_sym
    | LISTSXP of sxp_list
    | CLOSXP of sxp_clos
    | ENVSXP of sxp_env
    | PROMSXP of sxp_prom
    | LANGSXP of sxp_list
    | SPECIALSXP
    | BUILTINSXP of int
    | CHARSXP of string
    | LGLSXP of bool list
    | INTSXP of int list
    | REALSXP of float list
    | CPLXSXP
    | STRSXP of string list
    | DOTSXP
    | ANYSXP
    | VECSXP of t list
    | EXPRSXP
    | BCODESXP
    | EXTPTRSXP
    | WEAKREFSXP
    | RAWSXP
    | S4SXP
    | FUNSXP

  and sxp_sym  = { pname: t; sym_value: t; internal: t }
  and sxp_list = { carval: t; cdrval: t; tagval: t }
  and sxp_env  = { frame: t; (*enclos: t; hashtab: t*) }
  and sxp_clos = { formals: t; body: t; clos_env: t }
  and sxp_prom = { prom_value: t; expr: t; prom_env: t }

  let recursive x = Recursive (lazy (Lazy.force x))

  let build rec_build s =
    match sexptype s with
    | NilSxp     -> Val { content = NILSXP }
    | SymSxp     -> Val { content = SYMSXP {
        pname      = rec_build (inspect_symsxp_pname    s);
        sym_value  = rec_build (inspect_symsxp_value    s);
        internal   = rec_build (inspect_symsxp_internal s)}}
    | ListSxp    -> Val { content = LISTSXP {
        carval     = rec_build (inspect_listsxp_carval s);
        cdrval     = rec_build (inspect_listsxp_cdrval s);
        tagval     = rec_build (inspect_listsxp_tagval s)}}
    | CloSxp     -> Val { content = CLOSXP {
        formals    = rec_build (inspect_closxp_formals s);
        body       = rec_build (inspect_closxp_body    s);
        clos_env   = rec_build (inspect_closxp_env     s)}}
    | EnvSxp     -> Val { content = ENVSXP {
        frame      = rec_build (inspect_envsxp_frame   s);
     (* enclos     = rec_build (inspect_envsxp_enclos  s); *)
     (* hashtab    = rec_build (inspect_envsxp_hashtab s) *) }}
    | PromSxp    -> Val { content = PROMSXP {
        prom_value = rec_build (inspect_promsxp_value s);
        expr       = rec_build (inspect_promsxp_expr  s);
        prom_env   = rec_build (inspect_promsxp_env   s)}}
    | LangSxp    -> Val { content = LANGSXP {
        carval     = rec_build (inspect_listsxp_carval s);
        cdrval     = rec_build (inspect_listsxp_cdrval s);
        tagval     = rec_build (inspect_listsxp_tagval s)}}
    | SpecialSxp -> Val { content = SPECIALSXP }
    | BuiltinSxp -> Val { content = BUILTINSXP (inspect_primsxp_offset s)}
    | CharSxp    -> Val { content = CHARSXP (string_of_charsxp s) }
    | LglSxp     -> Val { content = LGLSXP (bool_list_of_lgl_vecsxp s)}
    | IntSxp     -> Val { content = INTSXP (int_list_of_int_vecsxp s)}
    | RealSxp    -> Val { content = REALSXP (float_list_of_real_vecsxp s)}
    | CplxSxp    -> Val { content = CPLXSXP }
    | StrSxp     -> Val { content = STRSXP (string_list_of_str_vecsxp s)}
    | DotSxp     -> Val { content = DOTSXP }
    | AnySxp     -> Val { content = ANYSXP }
    | VecSxp     -> Val { content = VECSXP (List.map rec_build (sexp_list_of_sexp_vecsxp s))}
    | ExprSxp    -> Val { content = EXPRSXP }
    | BcodeSxp   -> Val { content = BCODESXP }
    | ExtptrSxp  -> Val { content = EXTPTRSXP }
    | WeakrefSxp -> Val { content = WEAKREFSXP }
    | RawSxp     -> Val { content = RAWSXP }
    | S4Sxp      -> Val { content = S4SXP }
    | FunSxp     -> Val { content = FUNSXP }

end

module PrettyTypes = struct

  type t =
    | Recursive of t Lazy.t
    | NULL
    | SYMBOL of (string * t) option
    | ARG of string
    | PLACE
    | LIST of pairlist
    | CLOSURE of closure
    | ENV of environment
    | PROMISE of promise
    | CALL of t * pairlist
    | SPECIAL of int
    | BUILTIN
    | STRING of string
    | STRINGS of string list
    | INTS of int list
    | VECSXP of t list
    | BOOLS of bool list
    | FLOATS of float list
    | Unknown

  and closure     = { formals: t; body: t; clos_env: t }
  and environment = { frame: t; (* enclos: t; hashtab: t *) }
  and promise     = { value: t; expr: t; prom_env: t }

  and pairlist = (t * t) list (* For strict list parsing, t list. *)

  let recursive x = Recursive (lazy (Lazy.force x))

  exception Sexp_to_inspect of sexp
  exception Esoteric of sexp

  let symbol_of_symsxp builder (s : sym sxp) =
    let pname    = inspect_symsxp_pname    s
    and value    = inspect_symsxp_value    s
    and internal = inspect_symsxp_internal s in
    match (sexptype pname), (sexptype value), (sexptype internal) with
    | (NilSxp,  _, NilSxp) when sexp_equality s value -> SYMBOL None
    | (CharSxp, SymSxp, NilSxp) ->
        begin match (sexp_equality s value) &&
                    ("" = string_of_charsxp pname) with
        | true -> PLACE | false ->
        begin match (sexp_equality value (inspect_symsxp_value value))  &&
                    (NilSxp = sexptype (inspect_symsxp_pname value))    &&
                    (NilSxp = sexptype (inspect_symsxp_internal value)) with
        | true -> ARG (string_of_charsxp pname)
        | false -> raise (Esoteric s)
        end end
    | (CharSxp, _, NilSxp) ->
        let symbol_name = string_of_charsxp pname in
        SYMBOL (Some (symbol_name, (builder value)))
    | _ -> raise (Esoteric s)

  let rec list_of_listsxp builder s =
    let carval = inspect_listsxp_carval s
    and cdrval = inspect_listsxp_cdrval s
    and tagval = inspect_listsxp_tagval s in
    (* Strict parsing of the LIST:
    LIST begin match sexptype tagval with
    | NilSxp ->  (builder carval) :: begin
                 match builder cdrval with
                 | LIST l -> l | NULL -> []
                 | _ -> raise (Esoteric s) end
    | _ -> raise Esoteric end *)
    (* Lax parsing of the LIST: *)
    LIST begin ((builder tagval), (builder carval))::
      begin match builder cdrval with
      | LIST l -> l | NULL -> []
      | _ -> raise (Esoteric s) end
    end

  let rec build rec_build =
    let phi = fun f -> f (build rec_build) in
    function s -> match sexptype s with
    | NilSxp     -> NULL
    | SymSxp     -> begin try phi symbol_of_symsxp (Obj.magic s) with
                    | Esoteric _ -> Unknown end
    | ListSxp    -> begin try phi list_of_listsxp s with
                    | Esoteric _ -> Unknown end
    | CloSxp     -> CLOSURE {
        formals  = rec_build (inspect_closxp_formals s);
        body     = rec_build (inspect_closxp_body    s);
        clos_env = rec_build (inspect_closxp_env     s)}
    | EnvSxp     -> ENV {
        frame   = rec_build (inspect_envsxp_frame   s);
     (* enclos  = rec_build (inspect_envsxp_enclos  s); *) (* We do not care for now. *)
     (* hashtab = rec_build (inspect_envsxp_hashtab s)  *) }
    | PromSxp    -> PROMISE {
        value    = rec_build (inspect_promsxp_value s);
        expr     = rec_build (inspect_promsxp_expr  s);
        prom_env = rec_build (inspect_promsxp_env   s)}
    | LangSxp    ->
        let carval = inspect_listsxp_carval s
        and cdrval = inspect_listsxp_cdrval s
        and tagval = inspect_listsxp_tagval s in
        begin match build rec_build cdrval with
        | LIST l -> begin match sexptype tagval with
                    | NilSxp -> CALL ((build rec_build carval), l)
                    | _ -> Unknown end
        | _ -> Unknown end
    | SpecialSxp -> SPECIAL (inspect_primsxp_offset s)
    | BuiltinSxp -> BUILTIN
    | CharSxp    -> STRING  (string_of_charsxp s)
    | LglSxp     -> BOOLS   (bool_list_of_lgl_vecsxp s)
    | IntSxp     -> INTS    (int_list_of_int_vecsxp s)
    | RealSxp    -> FLOATS  (float_list_of_real_vecsxp s)
    | CplxSxp    -> Unknown
    | StrSxp     -> STRINGS (string_list_of_str_vecsxp s)
    | DotSxp     -> Unknown
    | AnySxp     -> Unknown
    | VecSxp     -> VECSXP  (List.map rec_build (sexp_list_of_sexp_vecsxp s))
    | ExprSxp    -> Unknown
    | BcodeSxp   -> Unknown
    | ExtptrSxp  -> Unknown
    | WeakrefSxp -> Unknown
    | RawSxp     -> Unknown
    | S4Sxp      -> Unknown
    | FunSxp     -> Unknown

end

module CParsed = Parsing (CTypes)

module PrettyParsed = Parsing (PrettyTypes)

module C = struct
  include CTypes
  include CParsed
end

module Pretty = struct
  include PrettyTypes
  include PrettyParsed
end

exception Parse_incomplete of string
let _ = Callback.register_exception "Parse_incomplete" (Parse_incomplete "any string")
exception Parse_error of string
let _ = Callback.register_exception "Parse_error" (Parse_error "any string")

external parse_sexp : string -> sexp = "parse_sexp"

(* The following exception needs to be registered
   in a callback when the R interpreter is initialised. *)
exception Runtime_error of lang sxp * string

external eval_langsxp : lang sxp -> sexp = "r_eval_sxp"

let eval_string s = eval_langsxp (parse_sexp s)

let rec prepare_args = function
  | (Some x)::l -> x::(prepare_args l)
  | None::l     -> prepare_args l
  | []          -> []

let arg f ?name x = Some (name, (Obj.magic (f x)))
let opt f name x = match x with
  | None -> None
  | Some x -> Some ((Some name), (Obj.magic (f x)))

let eval phi (args: (string option * sexp) option list) =
  eval_langsxp (langsxp phi (prepare_args args))
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

module type Interpreter = sig

  val loaded: unit

end

module Interpreter (Env : Environment) : Interpreter = struct

  let loaded = init ~name: Env.name
                    ~argv: Env.options
                    ~env:  Env.env
                    ~sigs: Env.signal_handlers
                    ()

end

module Base = struct
module List = struct

  class type t = object
    inherit S3.t
    method names : string list
  end

  class from_R r : t = object
    inherit S3.from_R r
    method names     = strings_of_t (get_attrib underlying "names")
  end

  let t_from_R r : t = new from_R r

end
module DataFrame = struct

  let subset2 = lazy (findfun (install "[[.data.frame"))

  class type t = object
    inherit List.t
    method row_names : string list
    method column : int -> sexp
    method element : int -> int -> sexp
  end

  class from_R r : t = object
    inherit List.from_R r
    method row_names = strings_of_t (get_attrib underlying "row.names")
    method column x = eval_langsxp (langsxp (Lazy.force subset2)
      [None, underlying; None, (int x)])
    method element x y = eval_langsxp (langsxp (Lazy.force subset2)
      [None, underlying; None, (int x); None, (int y)])
  end

  let t_from_R r : t = new from_R r

end
end
