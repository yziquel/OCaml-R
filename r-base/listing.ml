class listing_from_R r : R.Base.listing = object (self)
  inherit [R.Base.listing] R.s3_from_R r
  method names = R.strings_of_t (self#attribute "names")
end

