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

/* Converting data structures from/to R to/from OCaml. */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <Rinternals.h>
#include <R.h>
#include <Rdefines.h>
#include <Rembedded.h>
#include <R_ext/Parse.h>
#include <stdio.h>

void prerr_endline (char* s) {
  fprintf (stderr, "%s\n", s);
  fflush(stderr);
}

CAMLprim value Val_voidptr (void *pointer)
{
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc (1, Abstract_tag);
  Field(result, 0) = (value) pointer;
  CAMLreturn (result);
}

CAMLprim value Val_sexp (SEXP sexp) {
  CAMLparam0();
  CAMLlocal1(result);
  result=alloc(1,Abstract_tag) ;
  Field(result,0) = Val_long(sexp);
  CAMLreturn(result);
}

SEXP Sexp_val (value sexp) {
  SEXP s = (SEXP) Long_val(Field(sexp, 0));
  return s;
}

CAMLprim value sexptype_of_sexp (value sexp) {
  CAMLparam1(sexp);
  CAMLreturn(Val_int(TYPEOF(Sexp_val(sexp))));
}

#define SIMPLE_SEXP(name,type,setter,conv) \
CAMLprim value sexp_of_##name (value v) { \
  CAMLparam1(v); SEXP sexp ; \
  PROTECT(sexp = allocVector(type, 1)); \
  setter(sexp)[0] = conv(v) ; \
  UNPROTECT(1) ; CAMLreturn(Val_sexp(duplicate(sexp))); }

SIMPLE_SEXP(bool, LGLSXP, LOGICAL, Bool_val)
SIMPLE_SEXP(int, INTSXP, INTEGER, Int_val)
SIMPLE_SEXP(float, REALSXP, REAL, Double_val)

CAMLprim value sexp_of_string (value v) {
  CAMLparam1(v); SEXP sexp ;
  /* don't ask me why it is mkString and not mkChar... */
  PROTECT (sexp=mkString(String_val(v)));
  UNPROTECT(1);
  CAMLreturn(Val_sexp(duplicate(sexp)));
}

#define SIMPLE_VALUE(name, conv_r, conv_ml) \
CAMLprim value name##_of_sexp (value sexp) { \
  CAMLparam1(sexp); \
  CAMLlocal1(result); \
  SEXP e = (SEXP)Long_val(Field(sexp,0)); \
  PROTECT(e = Rf_eval(e,R_GlobalEnv)); \
  result=conv_ml(conv_r(e)); \
  UNPROTECT(1); \
  CAMLreturn(result);}

SIMPLE_VALUE(bool, Rf_asLogical, Val_bool)
SIMPLE_VALUE(int, Rf_asInteger, Val_int)
SIMPLE_VALUE(float, Rf_asReal, caml_copy_double)

CAMLprim value string_of_sexp (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  SEXP e = (SEXP)Long_val(Field(sexp,0));
  PROTECT(e = Rf_eval(AS_CHARACTER(e),R_GlobalEnv));
  result=caml_copy_string(CHARACTER_VALUE(e));
  UNPROTECT(1);
  CAMLreturn(result);
}


#define ARRAY_SEXP(name,type,setter,conv) \
CAMLprim value sexp_of_##name##_array (value v) { \
  CAMLparam1(v); SEXP sexp ; \
  if (Is_block(v) && Tag_val(v) == 0) { \
    int len = Wosize_val(v); int i ; \
    PROTECT(sexp = allocVector(type, len)); \
    for (i=0;i<len;i++) { \
      setter(sexp)[i] = conv(Field(v,i)); \
    } \
    UNPROTECT(1); \
  } else { \
    PROTECT(sexp = allocVector(type, 0)); \
    UNPROTECT(1); \
  } \
  CAMLreturn(Val_sexp(sexp)); \
}

ARRAY_SEXP(bool, LGLSXP, LOGICAL, Bool_val)
ARRAY_SEXP(int, INTSXP, INTEGER, Int_val)

CAMLprim value sexp_of_float_array (value v) {
  CAMLparam1(v);
  SEXP sexp ;
  if (Is_block(v) && Tag_val(v) == Double_array_tag) {
    int len = Wosize_val(v) / 2; int i ;
    PROTECT(sexp = allocVector(REALSXP, len));
    for (i=0;i<len;i++) {
      REAL(sexp)[i] = Double_field(v,i);
    }
    UNPROTECT(1);
  } else {
    PROTECT(sexp = allocVector(REALSXP, 0));
    UNPROTECT(1);
  }
  CAMLreturn(Val_sexp(sexp));
}

CAMLprim value sexp_of_string_array (value v) {
  CAMLparam1(v);
  SEXP sexp ;
  if (Is_block(v) && Tag_val(v) == 0) {
    int len = Wosize_val(v); int i ;
    PROTECT(sexp = allocVector(STRSXP, len));
    for (i=0;i<len;i++) {
      STRING_PTR(sexp)[i] = duplicate(mkChar(String_val(Field(v,i))));
    }
    UNPROTECT(1);
  } else {
    PROTECT(sexp = allocVector(STRSXP, 0));
    UNPROTECT(1);
  }
  CAMLreturn(Val_sexp(sexp));
}

#define ARRAY_VALUE(name, type, accessor, conv_r, conv_ml) \
CAMLprim value name##_array_of_sexp (value sexp) { \
  CAMLparam1(sexp); \
  CAMLlocal1(result); \
  SEXP e = (SEXP)Long_val(Field(sexp,0)); \
  type* e2 ; \
  PROTECT(e = Rf_eval(e,R_GlobalEnv)); \
  int length = LENGTH(e); \
  int i; \
  result = caml_alloc (length, 0); \
  for (i=0; i<length; i++) { \
    e2 = (type*)accessor(conv_r(e))[i]; \
    Store_field(result, i, conv_ml(e2)); \
  } \
  UNPROTECT(1); \
  CAMLreturn(result);}

/* ARRAY_VALUE(bool, int, LOGICAL, AS_LOGICAL, Val_bool) */
    /* Commented because 'cast to pointer from integer of different size' */

/* ARRAY_VALUE(int, int, INTEGER, AS_INTEGER, Val_int) */
    /* Commented because 'cast to pointer from integer of different size' */

//ARRAY_VALUE(string, char*, CHARACTER_VALUE, caml_copy_string)

/* Commented because 'passing argument 1 of ‘Rf_asChar’ from incompatible pointer type' */
/*CAMLprim value string_array_of_sexp (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  SEXP e = (SEXP)Long_val(Field(sexp,0));
  char** e2 ;
  PROTECT(e = Rf_eval(e,R_GlobalEnv));
  int length = LENGTH(e);
  int i;
  result = caml_alloc (length, 0);
  for (i=0; i<length; i++) {
    e2 = (char**)STRING_PTR(AS_CHARACTER(e))[i];
    Store_field(result, i, caml_copy_string(CHARACTER_VALUE(e2)));
  }
  UNPROTECT(1);
  CAMLreturn(result);
}*/

CAMLprim value float_array_of_sexp (value sexp) {
  CAMLparam1(sexp);
  CAMLlocal1(result);
  SEXP e = (SEXP)Long_val(Field(sexp,0));
  double d ;
  PROTECT(e = Rf_eval(e,R_GlobalEnv));
  int length = LENGTH(e);
  int i;
  result = caml_alloc (length*2, Double_array_tag);
  for (i=0; i<length; i++) {
    d = REAL(AS_NUMERIC(e))[i];
    Store_double_field(result, i, d);
  }
  UNPROTECT(1);
  CAMLreturn(result);
}


CAMLprim value r_get_attrib (value sexp, value attrib) {
  CAMLparam2(sexp,attrib);
  SEXP e = (SEXP)Long_val(Field(sexp,0));
  SEXP res;
  PROTECT(res = Rf_getAttrib(e, mkString(String_val(attrib))));
  UNPROTECT(1);
  CAMLreturn(Val_sexp(res));
}
/*
CAMLprim value to_matrix (value f, value sexp) {
  CAMLparam2(f, sexp);
  CAMLlocal1(result);
  SEXP e = (SEXP)Long_val(Field(sexp,0));
  int length = LENGTH(e);

  result = caml_alloc(length,0);

}
*/

/* langsxp_of_list creates a lisp-like list out of an
   OCaml list. langsxps, called pairlists, are not much
   used in R these days, except for internal matters,
   such as being an argument to R_tryEval.
   - Argument l is the OCaml list.
   - Argument n is the length of the OCaml list. */
CAMLprim value langsxp_of_list (value l, value n) {
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
    if (first_time) {first_time = 0;} else {t = CDR(t);}
    SEXP sexp_item = (SEXP) Long_val(Field(Field(l_cursor, 0), 0));
    SETCAR(t, sexp_item);
    l_cursor = Field(l_cursor, 1);
  }
  UNPROTECT(1);

  CAMLreturn(Val_sexp(s));
}
