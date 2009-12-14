module type Environment =
sig

  val name : string
  val options : string list
  val signal_handlers : bool
  val env : (string * string) list

end

