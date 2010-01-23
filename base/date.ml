class type date = object
  inherit [date] s3
  method as_float : float
  method as_date : CalendarLib.Calendar.Date.t
end

