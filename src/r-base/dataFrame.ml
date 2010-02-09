let subset2 = R.symbol ~generic: true "[[.data.frame"

class dataframe_from_R (r : R.Base.dataframe R.t) : R.Base.dataframe = object (self)
  inherit listing_from_R (Obj.magic r)
    (* We need a proper inheritance mechanism using polymorphic variants.
       This use of Obj.magic is not only ugly, it is plainly wrong. *)
  method row_names = R.strings_of_t (self#attribute "row.names")
  method column : 'a. int -> 'a R.t = fun x -> R.eval subset2 [
    R.arg (fun x -> x) underlying  ;
    R.arg R.int        x           ]
  method element : 'a. int -> int -> 'a R.t = fun x y -> R.eval subset2 [
    R.arg (fun x -> x) underlying     ;
    R.arg R.int        x              ;
    R.arg R.int        y              ]
end

