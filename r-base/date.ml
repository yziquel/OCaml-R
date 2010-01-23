class date_from_R r : R.Base.date = object (self)
  inherit [R.Base.date] R.s3_from_R r
  method as_float = R.float_of_t (Obj.magic underlying)
  method as_date = CalendarLib.Calendar.Date.from_unixfloat (86400. *. self#as_float)
end

