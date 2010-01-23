(* Wrapping S3 classes into Objective Caml. *)

(**  Class type for S3 objects in R. *)
class type ['a] s3 = object

  val underlying : 'a t
  (**  Access to the underlying R data structure. *)

  method attribute  : 'b. string -> 'b t
  (**  [attribute attr_name] returns the R data structure
    *  which is the object's attribute of name [attr_name].
    *  The typing of this method is deliberately unsafe, in
    *  order to allow the user to type things correctly. *)

  method attributes : (Specification.symbol * sexp) list
  (**  Returns the whole list of attributes of an S3 object. *)

  method classes    : string list
  (**  Returns the list of S3 classes that the object is
    *  an instance of. *)

end

class ['a] s3_from_R : 'a t -> ['a] s3
(**  Constructor of an [s3] object from an R S3 object. *)

