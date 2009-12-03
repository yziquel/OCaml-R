/* For the purpose of reverse-engineering, here's the stub for eval.
   I'll drop it afterwards, since R_tryEval seems the only solution
   to forward R errors to OCaml. */

CAMLprim value r_reveng_eval_sxp (value call) {
  CAMLparam1(call);
  CAMLreturn(Val_sexp(Rf_eval(Sexp_val(call), R_GlobalEnv)));
}

SEXP Rf_promiseArgs (SEXP el, SEXP rho);
CAMLprim value r_reveng_promise_args (value args) {
  CAMLparam1(args);
  CAMLreturn(Val_sexp(Rf_promiseArgs(Sexp_val(args), R_GlobalEnv)));
}

/* Following context definitions are copy-pasted from Defn.h. */

#include <setjmp.h>

//#ifdef HAVE_POSIX_SETJMP
# define SIGJMP_BUF sigjmp_buf
# define SIGSETJMP(x,s) sigsetjmp(x,s)
# define SIGLONGJMP(x,i) siglongjmp(x,i)
# define JMP_BUF sigjmp_buf
# define SETJMP(x) sigsetjmp(x,0)
# define LONGJMP(x,i) siglongjmp(x,i)
//#else
//# define SIGJMP_BUF jmp_buf
//# define SIGSETJMP(x,s) setjmp(x)
//# define SIGLONGJMP(x,i) longjmp(x,i)
//# define JMP_BUF jmp_buf
//# define SETJMP(x) setjmp(x)
//# define LONGJMP(x,i) longjmp(x,i)
//#endif


/* Evaluation Context Structure */
typedef struct RCNTXT {
    struct RCNTXT *nextcontext; /* The next context up the chain */
    int callflag;               /* The context "type" */
    JMP_BUF cjmpbuf;            /* C stack and register information */
    int cstacktop;              /* Top of the pointer protection stack */
    int evaldepth;              /* evaluation depth at inception */
    SEXP promargs;              /* Promises supplied to closure */
    SEXP callfun;               /* The closure called */
    SEXP sysparent;             /* environment the closure was called from */
    SEXP call;                  /* The call that effected this context*/
    SEXP cloenv;                /* The environment */
    SEXP conexit;               /* Interpreted "on.exit" code */
    void (*cend)(void *);       /* C "on.exit" thunk */
    void *cenddata;             /* data for C "on.exit" thunk */
    void *vmax;                 /* top of R_alloc stack */
    int intsusp;                /* interrupts are suspended */
    SEXP handlerstack;          /* condition handler stack */
    SEXP restartstack;          /* stack of available restarts */
    struct RPRSTACK *prstack;   /* stack of pending promises */
//#ifdef BYTECODE
//    SEXP *nodestack;
//# ifdef BC_INT_STACK
//    IStackval *intstack;
//# endif
//#endif
    SEXP srcref;                /* The source line in effect */
} RCNTXT, *context;

CAMLprim value r_global_context (value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  result = caml_alloc(1, Abstract_tag);
  Field(result, 0) = (value) R_GlobalContext;
  CAMLreturn(result);
}

CAMLprim value inspect_context_callfun (value cntxt) {
  CAMLparam1(cntxt);
  CAMLreturn(Val_sexp(((context) Field(cntxt, 0))->callfun));
}

CAMLprim value inspect_context_sysparent (value cntxt) {
  CAMLparam1(cntxt);
  CAMLreturn(Val_sexp(((context) Field(cntxt, 0))->sysparent));
}

/* The Various Context Types.

 * In general the type is a bitwise OR of the values below.
 * Note that CTXT_LOOP is already the or of CTXT_NEXT and CTXT_BREAK.
 * Only functions should have the third bit turned on;
 * this allows us to move up the context stack easily
 * with either RETURN's or GENERIC's or RESTART's.
 * If you add a new context type for functions make sure
 *   CTXT_NEWTYPE & CTXT_FUNCTION > 0
 */
enum {
    CTXT_TOPLEVEL = 0,
    CTXT_NEXT     = 1,
    CTXT_BREAK    = 2,
    CTXT_LOOP     = 3,  /* break OR next target */
    CTXT_FUNCTION = 4,
    CTXT_CCODE    = 8,
    CTXT_RETURN   = 12,
    CTXT_BROWSER  = 16,
    CTXT_GENERIC  = 20,
    CTXT_RESTART  = 32,
    CTXT_BUILTIN  = 64  /* used in profiling */
};

void Rf_begincontext (RCNTXT * cptr, int flags, SEXP syscall, SEXP env, SEXP sysp, SEXP promargs, SEXP callfun);
CAMLprim value r_reveng_begin_context_native (value flags, value syscall, value env, value sysp, value promargs, value callfun) {
  CAMLparam5(flags, syscall, env, sysp, promargs);
  CAMLxparam1(callfun);
  CAMLlocal1(result);
  result = caml_alloc(1, Abstract_tag);
  Field(result, 0) = (value) malloc(sizeof(RCNTXT));
  Rf_begincontext ( (context) Field(result, 0), Int_val(flags), Sexp_val(syscall), Sexp_val(env),
                    Sexp_val(sysp), Sexp_val(promargs), Sexp_val(callfun));
  CAMLreturn(result);
}
CAMLprim value r_reveng_begin_context_bytecode (value * argv, int argn) {
  return r_reveng_begin_context_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

void Rf_endcontext (RCNTXT * cptr);
CAMLprim value r_reveng_end_context (value cntxt) {
  CAMLparam1(cntxt);
  Rf_endcontext((context) Field(cntxt, 0));
  free((context) Field(cntxt, 0));  /* As Goswin Brederlow suggested, it may be a good idea to
                                       allocate the context on the C heap, wrap it up in an OCaml
                                       value with finalisers which call free() themselves. */
  CAMLreturn(Val_unit);
} 

SEXP Rf_matchArgs (SEXP formals, SEXP supplied, SEXP call);
CAMLprim value r_reveng_match_args (value formals, value supplied, value call) {
  CAMLparam3(formals, supplied, call);
  CAMLreturn(Val_sexp(Rf_matchArgs(Sexp_val(formals), Sexp_val(supplied), Sexp_val(call))));
}

SEXP Rf_NewEnvironment (SEXP namelist, SEXP valuelist, SEXP rho);
CAMLprim value r_reveng_new_environment (value namelist, value valuelist, value rho) {
  CAMLparam3(namelist, valuelist, rho);
  CAMLreturn(Val_sexp(Rf_NewEnvironment(Sexp_val(namelist), Sexp_val(valuelist), Sexp_val(rho))));
}

SEXP Rf_mkPROMISE (SEXP expr, SEXP rho);
CAMLprim value r_reveng_mkPROMISE (value expr, value rho) {
  CAMLparam2(expr, rho);
  CAMLreturn(Val_sexp(Rf_mkPROMISE(Sexp_val(expr), Sexp_val(rho))));
}

void (SET_MISSING) (SEXP x, int v);
CAMLprim value r_reveng_SET_MISSING (value x, value v) {
  CAMLparam2(x, v);
  (SET_MISSING) (Sexp_val(x), Int_val(v));
  CAMLreturn(Val_unit);
}

void Rf_defineVar (SEXP symbol, SEXP v, SEXP rho);
CAMLprim value r_reveng_define_var (value symbol, value v, value rho) {
  CAMLparam3(symbol, v, rho);
  Rf_defineVar(Sexp_val(symbol), Sexp_val(v), Sexp_val(rho));
  CAMLreturn(Val_unit);
}