module type Environment =
sig

  val name : string    (* This is the first argument of argv for R.
                          Mandatory, otherwise libR.so segfaults. *)

  (* See R reference manual, refman.pdf, page 452, section intitled
     'Startup - Initialization at Start of an R Session' for details
     about the most important command line options.
     More options are documented on the following webpage:
     http://cran.r-project.org/doc/manuals/R-intro.html#Invoking-R *)

  val options : string list

  (* signal_handlers, if set to false, asks R not to install its
     signal handlers. I've been experiencing weird issues with R signal
     handlers, since, for instance, a SIGSEGV originating from OCaml is
     caught by libR.so, and R asks then asks whether or not you want to
     save your workspace, et ceterae. By default, set to false. *)
  val signal_handlers : bool

  val env : (string * string) list

  val packages : string list option

end

