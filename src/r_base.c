/*********************************************************************************/
/*                OCaml-R                                                        */
/*                                                                               */
/*    Copyright (C) 2008-2009 Institut National de Recherche en                  */
/*    Informatique et en Automatique. All rights reserved.                       */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU General Public License as                    */
/*    published by the Free Software Foundation; either version 3 of the         */
/*    License, or  any later version.                                            */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU Library General Public License for more details.                       */
/*                                                                               */
/*    You should have received a copy of the GNU General Public                  */
/*    License along with this program; if not, write to the Free Software        */
/*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*********************************************************************************/

//#define OCAMLR_DEBUG   /* Toggle this on or off to enable debugging. */

#define _GNU_SOURCE    /* Required by asprinft (see 'man asprintf'), as it is a
                          GNU extension. Seems a bit overkill. Should try to
                          remove this dependency on asprintf. */

#define USE_RINTERNALS /* This compilation directive allows us to have access to
                          the definition of R internal types. Compilation of the
                          inspect* functions is otherwise prohibited. */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <callback.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rinterface.h>
#include <Rembedded.h>
#include <R_ext/Parse.h>
#include <stdio.h>


#ifdef OCAMLR_DEBUG

/* Debugging function. Prints on stderr. */

void prerr_endline (char* s) {
  fprintf (stderr, "%s\n", s);
  fflush(stderr);
}

#endif


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


/* Wrapping and unwrapping of R values. */

CAMLprim value Val_sexp (SEXP sexp) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, Abstract_tag);
  Field(result, 0) = (value) sexp;
    /* Do not use Val_long in the above statement,
       as it will drop the top bit. See mlvalues.h. */
  CAMLreturn(result);
}

SEXP Sexp_val (value sexp) {
  return (SEXP) Field(sexp, 0);
}

#define Val_vecsexp(x) Val_sexp(x)
#define Vecsexp_val(x) ((VECSEXP) Sexp_val(x))


/* Comparison operator. */
CAMLprim value r_sexp_equality (value s1, value s2) {
  CAMLparam2(s1, s2);
  CAMLreturn(Val_bool(Sexp_val(s1) == Sexp_val(s2)));
}


/* The NULL constant in R... */

CAMLprim value r_null (value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_sexp(R_NilValue));
}


/* Extracting runtime R low-level type information. */

CAMLprim value r_sexptype_of_sexp (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_int(TYPEOF(Sexp_val(sexp))));
}


/* Beta-reduction in R. */

/* r_langsxp_of_list creates a lisp-like list out of an
   OCaml list. langsxps, called pairlists, are not much
   used in R these days, except for internal matters,
   such as being an argument to R_tryEval.
   - Argument l is the OCaml list.
   - Argument n is the length of the OCaml list.
   Remark: LANGSXPs are essentially LISTSXPs with the
   type of the head element set to LANGSXP. */
CAMLprim value r_langsxp_of_list (value l, value n) {
  CAMLparam2(l, n);
  CAMLlocal1(l_cursor);
  SEXP s, t;

  PROTECT(t = s = allocList(Int_val(n)));
  SET_TYPEOF(s, LANGSXP);

  /* Fill in list s, with t moving over the pairlist,
     with values from l */
  int first_time = 1;
  l_cursor = l;
  while (l_cursor && Is_block(l_cursor)) {
    /* I've seen somewhere macros / functions allowing to dynamically
       allocate the next memory space of a listsxp. Will have to check
       that in more details later on. Would make code clearer. */
    if (first_time) {first_time = 0;} else {t = CDR(t);}
    SETCAR(t, Sexp_val(Field(l_cursor, 0)));
    l_cursor = Field(l_cursor, 1);
  }
  UNPROTECT(1);

  CAMLreturn(Val_sexp(s));
}

CAMLprim value r_eval_sxp (value sexp_list) {

  /* sexp_list is an OCaml value containing a SEXP of sexptype LANGSXP.
     This is a LISP-style pairlist of SEXP values. r_eval_sxp executes
     the whole string, and sends back the resulting SEXP wrapped up in
     an OCaml value. There's also an error handling mechanism.
     r_eval_sxp handles values of type LANGSXP and PROMSXP. So we have two
     functions on the OCaml side associated to this stub, the first on
     with type lang sexp -> raw sexp, the other one with type
     prom sexp -> raw sexp. This also means that their is a dynamic type
     checking being done in the scope of the R_tryEval function, and it
     would be nice to shortcut it with statically typed equivalents. */

  CAMLparam1(sexp_list);

  SEXP e;        // Placeholder for the result of beta-reduction.
  int error = 0; // Error catcher boolean.

  /* Should this be wrapped with a PROTECT() and
     an UNPROTECT(1), or not? */
  PROTECT(e = R_tryEval(Sexp_val(sexp_list), R_GlobalEnv, &error));
  UNPROTECT(1);

  /* Will have to implement error handling in OCaml. */
  if (error) caml_failwith("OCaml-R error in r_eval_sxp C stub.");

  CAMLreturn(Val_sexp(e));
}


/* Data conversion to and from OCaml and R. */

CAMLprim value r_internal_string_of_charsxp (value charsxp) {
  CAMLparam1(charsxp);
  /* Maxence Guesdon declares something like CAMLlocal1(result) for
     the output of caml_copy_string. To which extent is it necessary? */
  /* Moreover, it is yet unclear whether or not R strings are NULL
     terminated, or if they simply have a size fixed in the VECSEXP structure. */
  CAMLreturn(caml_copy_string(CHAR(Sexp_val(charsxp))));
}

CAMLprim value r_charsxp_of_string (value s) {
  /* Documentation for generating R strings can be found in "Writing R
     extensions", section 5.9.7 "Handling character data". */
  CAMLparam1(s);
  SEXP charsxp;
  PROTECT(charsxp = mkChar(String_val(s)));
  UNPROTECT(1);
  CAMLreturn(Val_sexp(charsxp));
}

CAMLprim value r_access_str_vecsxp (value strsxp, value offset) {
  CAMLparam2(strsxp, offset);
  /* Same comments as for r_accesas_int_vecsxp and for
     r_internal_string_of_charsxp. */
  CAMLreturn(caml_copy_string(CHAR(STRING_ELT(
    (char **) Vecsexp_val(strsxp), (Int_val(offset))))));
}

CAMLprim value r_strsxp_of_string (value s) {
  CAMLparam1(s);
  SEXP strsxp;
  PROTECT(strsxp = mkString(String_val(s)));
  UNPROTECT(1);
  CAMLreturn(Val_sexp(strsxp));
}

CAMLprim value r_access_int_vecsxp (value intsxp, value offset) {
  CAMLparam2(intsxp, offset);
  /* The R macro is #define INTEGER(x) ((int *) DATAPTR(x)).
     Should use Val_int, or int32s? More generally, the typing
     is here somewhat confusing (or confused)... Is offset an int? */
  CAMLreturn(Val_int(INTEGER((int *) Vecsexp_val(intsxp))[Int_val(offset)]));
}


CAMLprim value r_sexp_of_symbol (value symbol) {
  /* Remark: explanation of the install function at
     https://stat.ethz.ch/pipermail/r-devel/2009-August/054494.html */
  CAMLparam1(symbol);
  char* c_symbol = String_val(symbol);
  SEXP e;

#ifdef OCAMLR_DEBUG
  prerr_endline (c_symbol);
  PROTECT(e = duplicate(findVar(install(c_symbol), R_GlobalEnv)));
  PrintValue(e);
#else
  PROTECT(e = duplicate(findVar(install(c_symbol), R_GlobalEnv)));
#endif
  UNPROTECT(1);

  CAMLreturn(Val_sexp(e));
}

CAMLprim value r_sexp_of_string (value expression) {
  CAMLparam1(expression);
  char* c_name = "camlrtmp";
  char* s_exp;
  CAMLlocal1(result);
  SEXP e, tmp;
  int hadError;
  ParseStatus status;

  asprintf(&s_exp, "%s = %s", c_name, String_val(expression));
  PROTECT(tmp = mkString(s_exp));
  PROTECT(e = R_ParseVector(tmp, 1, &status, R_NilValue));
  /* PrintValue(e); DEBUG */
  R_tryEval(VECTOR_ELT(e,0), R_GlobalEnv, &hadError);
  UNPROTECT(2);
  free(s_exp);
  result = r_sexp_of_symbol(caml_copy_string(c_name));
  CAMLreturn(result);
}

CAMLprim value r_set_var (value name, value sexp) {
  CAMLparam2(name,sexp);
  char* c_name = String_val(name);
  SEXP e = (SEXP) Long_val(Field(sexp,0));
  setVar(install(c_name), e, R_GlobalEnv);
  CAMLreturn(Val_unit);
}

CAMLprim value r_print_value (value sexp) {
  CAMLparam1(sexp);
  SEXP e = Sexp_val(sexp);
  PrintValue(e);
  CAMLreturn(Val_unit);
}

/* Commented out because of 'warning: assignment makes pointer from integer without a cast' */
//CAMLprim value r_exec (value fun_name, value args) {
//  CAMLparam2(fun_name, args);
//  CAMLlocal2(tmpval,tmpval2);
//  int nb_args = Wosize_val(args);
//  int i;
//  SEXP s, t, u;
//  int error = 0;
//  char* error_msg;
///*  prerr_endline (asprintf("%d args", nb_args));*/
//
//  PROTECT(t = s = allocList(nb_args + 1));
//  SET_TYPEOF(s, LANGSXP);
//  SETCAR(t, install(String_val(fun_name)));
//  for (i = 0; i < nb_args; i++) {
//    t = CDR(t);
//    tmpval = Field(args,i);
//    if (Is_block(tmpval)) {
//      tmpval2 = Field(tmpval,1);
//      if (Field(tmpval,0) == hash_variant("Named")) {
///*
//        prerr_endline("Named");
//        prerr_endline(String_val(Field(tmpval2,0)));
//*/
//        SET_TAG(t, install(String_val(Field(tmpval2,0))));
//        u = Long_val(Field(Field(tmpval2,1),0));
///*
//        PrintValue(u);
//*/
//        SETCAR(t, duplicate(u));
///*
//        prerr_endline("Named ok");
//*/
//      } else if (Field(tmpval,0) == hash_variant("Anon")) {
///*
//        prerr_endline ("Anon");
//*/
//        u = Long_val(Field(tmpval2,0));
///*
//        PrintValue(u);
//*/
//        SETCAR(t, duplicate(u));
///*
//        prerr_endline ("Anon ok");
//*/
//      } else {
//        prerr_endline("bad constructor");
//        /* bad constructor */
//      }
//
//    }  else
//    {
//      /* no argument, bad value for us */
//      prerr_endline("no block, bad value");
//    }
//  }
//  R_tryEval(s, R_GlobalEnv, &error);
//  UNPROTECT(1);
//  if (error) {
//    asprintf(&error_msg, "R failure while calling function '%s'.", String_val(fun_name));
//    caml_failwith(error_msg);
//  }
//  CAMLreturn(Val_unit);
//}


/* What follows is low-level accessor functions, in order to inspect
   in details the contents of SEXPs and VECSEXPs. The implementation
   details are in Rinternals.h, enclosed in #ifdef USE_RINTERNALS
   directives, making these opaque pointers when not turned on. */

/* Concerning VECSEXPs: */

/* Quotation from section 1.1.3 "The 'data'" of "R Internals" (R-ints.pdf):
   'This [i.e. vecsxp.truelength] is almost unused. The only current use is for
   hash tables of environments (VECSXPs), where length is the size of the table
   and truelength is the number of primary slots in use, and for the reference
   hash tables in serialization (VECSXPs),where truelength is the number of
   slots in use.' */

/* CHARSXPs are in fact VECSEXPs. Concerning encoding, quotation from section 1.1.2
   "Rest_of_header" of "R internals" (R-ints.pdf): 'As from R 2.5.0, bits 2 and 3 for
   a CHARSXP are used to note that it is known to be in Latin-1 and UTF-8 respectively.
   (These are not usually set if it is also known to be in ASCII, since code does not
   need to know the charset to handle ASCII strings. From R 2.8.0 it is guaranteed that
   they will not be set for CHARSXPs created by R itself.) As from R 2.8.0 bit 5 is used
   to indicate that a CHARSXP is hashed by its address, that is NA STRING or in the
   CHARSXP cache. */


CAMLprim value inspect_vecsxp_length (value vecsexp) {
  CAMLparam1(vecsexp);
  CAMLreturn(Val_int(Vecsexp_val(vecsexp)->vecsxp.length));
}

/*CAMLprim value inspect_vecsxp_truelength (value vecsexp) {
  CAMLparam1(vecsexp);
  CAMLreturn(Val_int(Vecsexp_val(vecsexp)->vecsxp.truelength));
}*/

/* Concerning various types of SEXPs: */

CAMLprim value inspect_primsxp_offset (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_int(Sexp_val(sexp)->u.primsxp.offset));
}

CAMLprim value inspect_symsxp_pname (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.symsxp.pname));
}

CAMLprim value inspect_symsxp_value (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.symsxp.value));
}

CAMLprim value inspect_symsxp_internal (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.symsxp.internal));
}

CAMLprim value inspect_listsxp_carval (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.listsxp.carval));
}

CAMLprim value inspect_listsxp_cdrval (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.listsxp.cdrval));
}

CAMLprim value inspect_listsxp_tagval (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.listsxp.tagval));
}

CAMLprim value inspect_envsxp_frame (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.envsxp.frame));
}

CAMLprim value inspect_envsxp_enclos (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.envsxp.enclos));
}

CAMLprim value inspect_envsxp_hashtab (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.envsxp.hashtab));
}

CAMLprim value inspect_closxp_formals (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.closxp.formals));
}

CAMLprim value inspect_closxp_body (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.closxp.body));
}

CAMLprim value inspect_closxp_env (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.closxp.env));
}

CAMLprim value inspect_promsxp_value (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.promsxp.value));
}

CAMLprim value inspect_promsxp_expr (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.promsxp.expr));
}

CAMLprim value inspect_promsxp_env (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_sexp(Sexp_val(sexp)->u.promsxp.env));
}

CAMLprim value parse_sexp (value s) {
  CAMLparam1(s);
  SEXP text ;
  SEXP pr ;
  ParseStatus status;
  PROTECT(text = mkString(String_val(s)));
  PROTECT(pr=R_ParseVector(text, 1, &status, R_NilValue));
  UNPROTECT(2);
  switch (status) {
    case PARSE_OK:
     CAMLreturn(Val_sexp(VECTOR_ELT(pr,0)));
    case PARSE_INCOMPLETE:
    case PARSE_EOF:
      caml_raise_with_string(*caml_named_value("Parse_incomplete"), (String_val(s)));
    case PARSE_NULL:
    case PARSE_ERROR:
      caml_raise_with_string(*caml_named_value("Parse_error"), (String_val(s)));
      }
}
