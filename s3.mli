(* Wrapping S3 classes into Objective Caml. *)

module S3 : sig

  (**  Class type for S3 objects in R. *)
  class type t = object

    method underlying : sexp
    (**  Access to the underlying R data structure. *)

    method attribute  : string -> sexp
    (**  [attribute attr_name] returns the R data structure
      *  which is the object's attribute of name [attr_name]. *)

    method attributes : (Specification.symbol * sexp) list
    (**  Returns the whole list of attributes of an S3 object. *)

    method classes    : string list
    (**  Returns the list of S3 classes that the object is
      *  an instance of. *)

  end

  class from_R : sexp -> t
  (**  Constructor of an [S3.t] object from an R S3 object. *)

  val t_from_R : sexp -> t
  (**  A handy shortcut for the [from_R] class constructor. *)

end

