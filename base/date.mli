(**  The class type {!R.Base.date} wraps up an R date
  *  into an OCaml object. *)
class type date = object

  (**  An R date is an S3 object. *)
  inherit [date] s3

  (**  Converts the date to a float. *)
  method as_float : float

  (**  Converts the date to a Calendar date. *)
  method as_date : CalendarLib.Calendar.Date.t

end

