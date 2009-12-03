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
