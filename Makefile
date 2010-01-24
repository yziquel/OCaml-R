RLIBDIR=/usr/lib/R/lib
RINCLUDES=-I . -I /usr/share/R/include
INCLUDES=       -I +ocamldoc -I `ocamlc -where`/caml $(RINCLUDES)

COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES) -ccopt -L$(RLIBDIR) -cclib -lR
LINKFLAGS_BYTE=$(INCLUDES) -ccopt -L$(RLIBDIR)  -cclib -lR

FLAVOUR=INTERNAL

all: build build-math

build-math:
	make -C math

build: r.cma r.cmxa r.cmxs oCamlR.cmo oCamlR.cmx rbase.cma rbase.cmxa

r.cma: dllr_stubs.so r.cmo
	ocamlc -verbose -a -dllpath /usr/lib/R/lib -dllib dllr_stubs.so -dllib libR.so -o r.cma r.cmo

r.cmxa: dllr_stubs.so r.cmx
	ocamlopt -verbose -a -ccopt -L/usr/lib/R/lib -cclib -lr_stubs -cclib -lR -o r.cmxa r.cmx

r.cmxs: r.cmxa
	ocamlopt -shared -linkall -ccopt -L/usr/lib/R/lib -ccopt -L. -o $@ $<

rbase.cma: rbase.cmo
	ocamlc -verbose -a -o rbase.cma rbase.cmo

rbase.cmxa: rbase.cmx
	ocamlopt -verbose -a -o rbase.cmxa rbase.cmx

r.cmi: r.mli
	ocamlfind ocamlc -package calendar -verbose -c r.mli

r.cmo: r.ml r.cmi
	ocamlfind ocamlc -package calendar -verbose -package unix -c r.ml

r.cmx: r.ml r.cmi
	ocamlfind ocamlopt -package calendar -verbose -package unix -c r.ml

oCamlR.cmo:
	ocamlfind ocamlc -verbose -c oCamlR.ml

oCamlR.cmx:
	ocamlfind ocamlopt -verbose -c oCamlR.ml

rbase.cmi: rbase.mli
	ocamlfind ocamlc -package calendar -verbose -c rbase.mli

rbase.cmo: rbase.ml rbase.cmi
	ocamlfind ocamlc -package calendar -verbose -c rbase.ml

rbase.cmx: rbase.ml rbase.cmi
	ocamlfind ocamlopt -package calendar -verbose -c rbase.ml

r.ml: standard.ml base.ml
	cat                     \
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
	  base.ml               \
	> r.ml

r.mli: base.mli
	cat                     \
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
	  reduction.mli         \
	  initialisation.mli    \
	  base.mli              \
	> r.mli

standard.ml: standard.R
	R --silent --vanilla --slave < standard.R > standard.ml

base.ml:
	cat                     \
	  base/incipit.ml       \
	  base/listing.ml       \
	  base/dataFrame.ml     \
	  base/date.ml          \
	  base/excipit.ml       \
	> base.ml

base.mli:
	cat                     \
	  base/incipit.mli      \
	  base/listing.mli      \
	  base/dataFrame.mli    \
	  base/date.mli         \
	  base/excipit.mli      \
	> base.mli

rbase.ml:
	cat                     \
	  r-base/main.ml        \
	  r-base/listing.ml     \
	  r-base/dataFrame.ml   \
	  r-base/date.ml        \
	> rbase.ml

rbase.mli:
	cat                     \
	  r-base/main.mli       \
	  r-base/listing.mli    \
	  r-base/dataFrame.mli  \
	  r-base/date.mli       \
	> rbase.mli

r_stubs.o: r_stubs.c
	ocamlopt -verbose -ccopt -Wall $(COMPFLAGS) -ccopt -fPIC -c $<

libr_stubs.a: r_stubs.o
	ar rcs libr_stubs.a r_stubs.o

dllr_stubs.so: libr_stubs.a r_stubs.o
	ocamlmklib -verbose -o r_stubs r_stubs.o

clean:
	make -C math clean
	rm -f standard.ml base.ml base.mli r.ml r.mli rbase.ml rbase.mli
	rm -f *.o *.so *.a *.cmi *.cmo *.cmx *.cma *.cmxa *.cmxs

test: build
	ocaml -init ocamlinit

install: remove
	ocamlfind install R META *.a *.cm[ai] *.cmxa dllr_stubs.so

remove:
	ocamlfind remove R

