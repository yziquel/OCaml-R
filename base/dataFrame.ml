class type dataframe = object
  inherit listing
  method row_names : string list
  method column : int -> 'a t
  method element : int -> int -> 'a t
end

