module type Environment =
sig

  val name : string
  (**  This is the name of the first argument of argv for R.
       Mandatory, otherwise libR.so segfaults immediately. *)

  val options : string list
  (**  Other command line options passed to the libR.so library when
    *  initialising R.
    *
    *  See R reference manual, refman.pdf, page 452, section intitled
    *  'Startup - Initialization at Start of an R Session' for details
    *  about the most important command line options.
    *  More options are documented on the following webpage:
    *  http://cran.r-project.org/doc/manuals/R-intro.html#Invoking-R
    *)

  val signal_handlers : bool
  (**  If set to false, asks R not to install its signal handlers. I've
    *  been experiencing weird issues with R signal handlers, since, for
    *  instance, a SIGSEGV is sometimes caught by libR.so, and R asks then
    *  asks whether or not you want to save your workspace, et ceterae. By
    *  default, set to false.
    *)

  val env : (string * string) list
  (**  These are environment variables that needs to be set before
    *  initialising the R interpreter. In the [Standard] module, these
    *  values are determined when the binding itself is compiled.
    *)

end
