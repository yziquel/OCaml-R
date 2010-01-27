(**  The class type {!R.Base.listing} wraps up R lists
  *  into an OCaml object. The name [listing] was chosen
  *  to avoid a name class with OCaml's lists. *)
class type listing = object

  (**  A list is an S3 object. *)
  inherit [listing] s3

  (**  Names of the elements of the list. *)
  method names : string list

end

