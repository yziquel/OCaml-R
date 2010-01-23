module Stub = struct

  let sample = R.symbol "sample"

end

let sample (x : 'a list R.t) size ?replace ?(prob: float list option) () : 'a list R.t =
  (* Note that size may be left out in this R function. This
     behaviour does not fit the R.arg behaviour, nor does it
     fit the R.opt behaviour (since the argument should be
     named. This type of argument has to be worked out... *)
  R.eval Stub.sample [
    (R.arg (fun x -> x)             x)      ;
    (R.arg R.int                    size)   ;
    (R.opt R.bool         "replace" replace);
    (R.opt R.floats       "prob"    prob)   ]

class listing_from_R r : R.Base.listing = object (self)
  inherit [R.Base.listing] R.s3_from_R r
  method names = R.strings_of_t (self#attribute "names")
end

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

class date_from_R r : R.Base.date = object (self)
  inherit [R.Base.date] R.s3_from_R r
  method as_float = R.float_of_t (Obj.magic underlying)
  method as_date = CalendarLib.Calendar.Date.from_unixfloat (86400. *. self#as_float)
end

