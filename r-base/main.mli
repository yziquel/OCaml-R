(**  Sampling function. *)
val sample : 'a list R.t -> int -> ?replace: bool -> ?prob: float list -> unit -> 'a list R.t

