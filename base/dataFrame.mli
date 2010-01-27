(**  The class type {!R.Base.dataframe} wraps up R data
  *  frames into an OCaml object. *)
class type dataframe = object

  (**  Essentially, a dataframe is a list of columns. *)
  inherit listing

  (**  [df#column n] is the [n{^ th}] column of the data frame. *)
  method column : int -> 'a t

  (**  [df#element i j] is the [i{^ th}] element of the [j{^ th}] column. *)
  method element : int -> int -> 'a t

  (**  Retrieves the names of the rows of the data frame. *)
  method row_names : string list

end

