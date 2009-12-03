/* Stub code initialising and terminating the R interpreter. */

CAMLprim value init_r (value argv, value sigs) {

  /* -1- argv is an OCaml array of strings, which gives the command line
     arguments used to invoke the R interpreter. Code segfaults if
     the array does not contain a first element, which is the name of
     the program, typically "R", or "OCaml-R". Other arguments typically
     are "--vanilla", "--slave"...

     -2- sigs is an OCaml int. When set to 0, R signal handlers are not
     removed. When set to, say, 1, R signal handlers are removed. It is
     very useful to remove signal handlers when embedding R. */

  CAMLparam2(argv, sigs);
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
  CAMLreturn(Val_int(i));
}

CAMLprim value end_r (value unit) {
  /* This function terminates the R interpreter. It is not clear whether
     or not this function is garbage-collector-friendly. For details, see
     http://old.nabble.com/Reset-an-embedded-R.dll-td17236931.html */
  CAMLparam1(unit);
  Rf_endEmbeddedR(0);
  CAMLreturn(Val_unit);
}

