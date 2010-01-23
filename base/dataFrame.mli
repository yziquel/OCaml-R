class type dataframe = object
  inherit listing
  method column : int -> 'a t
  method element : int -> int -> 'a t
  method row_names : string list
end

