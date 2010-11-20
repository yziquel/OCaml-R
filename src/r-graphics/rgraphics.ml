let () = ignore (R.eval_string "require(graphics, quietly=TRUE)")

module Stub = struct

  let hist = R.symbol "hist"

end
