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

/* Debugging function. Prints on stderr. */
//void prerr_endline (char* s);

/* Wrapping and unwrapping of R values. */
CAMLprim value Val_sexp (SEXP sexp);
SEXP Sexp_val (value sexp);

/* Extracting runtime R low-level type information. */
CAMLprim value sexptype_of_sexp (value sexp);

/* The NULL constant in R... */
CAMLprim value r_null (value unit);


/* Conversion functions. */

#define SIMPLE_SEXP(name,type,setter,conv) \
CAMLprim value sexp_of_##name (value v);

SIMPLE_SEXP(bool, LGLSXP, LOGICAL, Bool_val)
SIMPLE_SEXP(int, INTSXP, INTEGER, Int_val)
SIMPLE_SEXP(float, REALSXP, REAL, Double_val)

CAMLprim value sexp_of_string (value v);

#define SIMPLE_VALUE(name, conv_r, conv_ml) \
CAMLprim value name##_of_sexp (value sexp);

SIMPLE_VALUE(bool, Rf_asLogical, Val_bool)
SIMPLE_VALUE(int, Rf_asInteger, Val_int)
SIMPLE_VALUE(float, Rf_asReal, caml_copy_double)

CAMLprim value string_of_sexp (value sexp);


#define ARRAY_SEXP(name,type,setter,conv) \
CAMLprim value sexp_of_##name##_array (value v);

ARRAY_SEXP(bool, LGLSXP, LOGICAL, Bool_val)
ARRAY_SEXP(int, INTSXP, INTEGER, Int_val)

CAMLprim value sexp_of_float_array (value v);
CAMLprim value sexp_of_string_array (value v);

#define ARRAY_VALUE(name, type, accessor, conv_r, conv_ml) \
CAMLprim value name##_array_of_sexp (value sexp);

/* ARRAY_VALUE(bool, int, LOGICAL, AS_LOGICAL, Val_bool) */
    /* Commented because 'cast to pointer from integer of different size' */

/* ARRAY_VALUE(int, int, INTEGER, AS_INTEGER, Val_int) */
    /* Commented because 'cast to pointer from integer of different size' */

//ARRAY_VALUE(string, char*, CHARACTER_VALUE, caml_copy_string)

/* Commented because 'passing argument 1 of ‘Rf_asChar’ from incompatible pointer type' */
/*CAMLprim value string_array_of_sexp (value sexp);*/

CAMLprim value float_array_of_sexp (value sexp);


CAMLprim value r_get_attrib (value sexp, value attrib);

/*CAMLprim value to_matrix (value f, value sexp);*/


/* langsxp_of_list creates a lisp-like list out of an
   OCaml list. langsxps, called pairlists, are not much
   used in R these days, except for internal matters,
   such as being an argument to R_tryEval.
   - Argument l is the OCaml list.
   - Argument n is the length of the OCaml list. */
CAMLprim value langsxp_of_list (value l, value n);
