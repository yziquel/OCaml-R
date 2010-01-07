(**  This is the [R] module of the OCaml-R package. It encapsulates the
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
