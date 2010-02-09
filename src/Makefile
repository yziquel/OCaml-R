include ../master.Makefile

all: opt byte
opt: libopt mathopt
byte: lib math

libopt: r.cmxa r.cmxs oCamlR.cmx rbase.cmxa rstats.cmxa rmath.cmxa
lib: r.cma oCamlR.cmo rbase.cma rstats.cma rmath.cma

mathopt: rmath.cmxa
math: rmath.cma

rmath.cma: dllrmath_stubs.so math/rmath.cmo
	$(OCAMLC) -verbose -a -dllib dllrmath_stubs.so -dllib libRmath.so -o rmath.cma math/rmath.cmo

rmath.cmxa: dllrmath_stubs.so math/rmath.cmx
	$(OCAMLOPT) -verbose -a -cclib -lrmath_stubs -cclib -lRmath -o rmath.cmxa math/rmath.cmx

librmath_stubs.a: math/rmath_stubs.o
	ar rcs librmath_stubs.a math/rmath_stubs.o

dllrmath_stubs.so: librmath_stubs.a math/rmath_stubs.o
	ocamlmklib -verbose -o rmath_stubs math/rmath_stubs.o

math/rmath_stubs.o: math/rmath_stubs.c

r.cma: dllr_stubs.so r.cmo
	$(OCAMLC) -verbose -a -dllpath $(RLIBDIR) -dllib dllr_stubs.so -dllib libR.so -o r.cma r.cmo

r.cmxa: dllr_stubs.so r.cmx
	$(OCAMLOPT) -verbose -a -ccopt -L$(RLIBDIR) -cclib -lr_stubs -cclib -lR -o r.cmxa r.cmx

rbase.cma: rbase.cmo
	$(OCAMLC) -verbose -a -o rbase.cma rbase.cmo

rbase.cmxa: rbase.cmx
	$(OCAMLOPT) -verbose -a -o rbase.cmxa rbase.cmx

rstats.cma: rstats.cmo
	$(OCAMLC) -verbose -a -o rstats.cma rstats.cmo

rstats.cmxa: rstats.cmx
	$(OCAMLOPT) -verbose -a -o rstats.cmxa rstats.cmx

r.ml: \
	  r_Env.ml              \
	  standard.ml           \
	  sexptype.ml           \
	  sexprec.ml            \
	  data.ml               \
	  allocation.ml         \
	  read_internal.ml      \
	  write_internal.ml     \
	  lazy.ml               \
	  symbols.ml            \
	  conversion.ml         \
	  internal.ml           \
	  s3.ml                 \
	  s4.ml                 \
	  parser.ml             \
	  reduction.ml          \
	  initialisation.ml     \
	  base.ml
	cat $^ > $@

r.mli: \
	  incipit.mli           \
	  r_Env.mli             \
	  standard.mli          \
	  sexptype.mli          \
	  data.mli              \
	  symbols.mli           \
	  conversion.mli        \
	  internal.mli          \
	  s3.mli                \
	  s4.mli                \
	  parser.mli            \
	  reduction.mli         \
	  initialisation.mli    \
	  base.mli
	cat $^ > $@

standard.ml: standard.R
	R --silent --vanilla --slave < $< > $@

base.ml: \
    base/incipit.ml       \
	  base/listing.ml       \
	  base/dataFrame.ml     \
	  base/date.ml          \
	  base/excipit.ml
	cat $^ > $@

base.mli: \
	  base/incipit.mli      \
	  base/listing.mli      \
	  base/dataFrame.mli    \
	  base/date.mli         \
	  base/excipit.mli
	cat $^ > $@

rbase.ml: \
	  r-base/main.ml        \
	  r-base/listing.ml     \
	  r-base/dataFrame.ml   \
	  r-base/date.ml
	cat $^ > $@

rbase.mli: \
	  r-base/main.mli       \
	  r-base/listing.mli    \
	  r-base/dataFrame.mli  \
	  r-base/date.mli
	cat $^ > $@

rstats.ml: r-stats/main.ml
	cat $^ > $@

rstats.mli: r-stats/main.mli
	cat $^ > $@

libr_stubs.a: r_stubs.o
	ar rcs libr_stubs.a r_stubs.o

dllr_stubs.so: libr_stubs.a r_stubs.o
	$(OCAMLMKLIB) -verbose -o r_stubs r_stubs.o

clean:
	$(MAKE) -C math clean
	rm -f standard.ml base.ml base.mli r.ml r.mli rbase.ml rbase.mli rstats.ml rstats.mli
	rm -f *.o *.so *.a *.cmi *.cmo *.cmx *.cma *.cmxa *.cmxs *.annot

test: build
	$(OCAML) -init ocamlinit

install: remove
	$(OCAMLFIND) install R META *.a *.cm[ai] *.cmxa dllr_stubs.so

remove:
	$(OCAMLFIND) remove R

