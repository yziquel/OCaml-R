/* Stub code initialising and terminating the R interpreter. */

/**  Initialises the R interpreter in the libR.so library.
  *
  *  @param argv An OCaml array of strings, which gives the command
  *         line arguments used to invoke the R interpreter. Code
  *         segfaults if the array does not contain a first element,
  *         which is the name of the program, typically "R", or
  *         "OCaml-R". Other arguments typically are "--vanilla",
  *         "--slave"...
  *  @param sigs An OCaml integer. When set to 0, R signal handlers
  *         are not removed. When set, for example, to 1, R signal
  *         handlers are removed. It is very useful to remove signal
  *         handlers when embedding R. Requires R >= 2.3.1.
  *  @return 1 if R is correctly initialised.
  */
CAMLprim value ocamlr_initEmbeddedR (value argv, value sigs) {

  int length = Wosize_val(argv);
  char* argv2[length];
  int i;

  // We duplicate the OCaml array into a C array.
  for (i=0; i<length; i++) argv2[i]=String_val(Field(argv, i));

  /* Don't let R set up its own signal handlers when sigs = 1.
     This requires R >= 2.3.1. */
  if (Int_val(sigs)) R_SignalHandlers = 0;

  // This is the libR.so function.
  i = Rf_initEmbeddedR(length, argv2);

  // Returns 1 if R is correctly initialised.
  return Val_int(i);
}


/**  Terminates an R interpreter. */
CAMLprim value ocamlr_endEmbeddedR (value unit) {
  /* This function terminates the R interpreter. It is not clear whether
     or not this function is garbage-collector-friendly. For details, see
     http://old.nabble.com/Reset-an-embedded-R.dll-td17236931.html */
  Rf_endEmbeddedR(0);
  return Val_unit;
}

