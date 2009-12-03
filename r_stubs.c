//#define OCAMLR_DEBUG   /* Toggle this on or off to enable debugging. */

#define _GNU_SOURCE    /* Required by asprinft (see 'man asprintf'), as it is a
                          GNU extension. Seems a bit overkill. Should try to
                          remove this dependency on asprintf. */

#define USE_RINTERNALS /* This compilation directive allows us to have access to
                          the definition of R internal types. Compilation of the
                          inspect* functions is otherwise prohibited. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
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


#include "initialisation.c"
#include "databridge.c"
#include "sexp.c"
#include "allocation.c"
#include "read_internal.c"
#include "write_internal.c"
#include "symbols.c"
#include "conversion.c"
#include "reduction.c"
#include "parser.c"



/* Code to audit. */

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


/********************************************/
/* For reverse-engineering purposes ONLY!!! */
/********************************************/

/* This is taken from memory.h */
static SEXPREC OCamlR_UnmarkedNode;

// Rinternals.h contains the macro definition:
// #define MARK(x)            ((x)->sxpinfo.mark)
//
//#define NODE_IS_MARKED(s) (MARK(s)==1)
//#define MARK_NODE(s) (MARK(s)=1)
//#define UNMARK_NODE(s) (MARK(s)=0)
//
// This is set in function InitMemory()...
//
// So we'll simply bother calling:

CAMLprim value r_init_ocaml_node (value unit) {
  CAMLparam1(unit);
  OCamlR_UnmarkedNode.sxpinfo.mark = 0;
  CAMLreturn(Val_unit);
}


/* Now the real code: */

CAMLprim value r_sexp_allocate (value unit) {
  /* We should be using allocSExp from memory.c instead. This would take
     care of all the stupid stuff up there concerning UnmarkedNode,
     garbage collection and tutti quanti. */
  CAMLparam1(unit);
  CAMLreturn(Val_sexp((SEXP) Calloc(1, SEXPREC)));
}

#define PRCODE(x)	((x)->u.promsxp.expr)
#define PRENV(x)	((x)->u.promsxp.env)
#define PRVALUE(x)       ((x)->u.promsxp.value)
#define PRSEEN(x)        ((x)->sxpinfo.gp)
CAMLprim value r_write_promise (value p, value expression) {
  CAMLparam2(p, expression);
  SEXP r_p = Sexp_val(p);
  r_p->sxpinfo = OCamlR_UnmarkedNode.sxpinfo;
  TYPEOF(r_p) = PROMSXP;
  PRCODE(r_p) = Sexp_val(expression);
  PRENV(r_p) = R_GlobalEnv;
  PRVALUE(r_p) = R_UnboundValue;
  PRSEEN(r_p) = 0;
  ATTRIB(r_p) = R_NilValue;
  CAMLreturn(Val_unit);
}
