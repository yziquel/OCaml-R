RLIBDIR=/usr/lib/R/lib
RINCLUDES=-I . -I /usr/share/R/include
INCLUDES=       -I +ocamldoc -I `ocamlc -where`/caml $(RINCLUDES)

COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES) -ccopt -L$(RLIBDIR) -cclib -lR
LINKFLAGS_BYTE=$(INCLUDES) -ccopt -L$(RLIBDIR)  -cclib -lR

all: build

build: r.cma r.cmxa oCamlR.cma oCamlR.cmxa

r.cma: dllr_stubs.so r.cmo
	ocamlc -a -dllpath /usr/lib/R/lib -dllib dllr_stubs.so -dllib libR.so -o r.cma r.cmo

r.cmxa: dllr_stubs.so r.cmx
	ocamlopt -a -ccopt -L/usr/lib/R/lib -cclib -lr_stubs -cclib -lR -o r.cmxa r.cmx

oCamlR.cma: oCamlR.cmo
	ocamlc -a -o oCamlR.cma oCamlR.cmo

oCamlR.cmxa: oCamlR.cmx
	ocamlopt -a -o oCamlR.cmxa oCamlR.cmx

r.cmo: r.ml
	ocamlfind ocamlc -package unix -c r.ml

r.cmx: r.ml
	ocamlfind ocamlopt -package unix -c r.ml

oCamlR.cmo:
	ocamlfind ocamlc -c oCamlR.ml

oCamlR.cmx:
	ocamlfind ocamlopt -c oCamlR.ml

r.ml: standard.ml
	cat r_Env.ml standard.ml initialisation.ml sexptype.ml sexprec.ml data.ml allocation.ml read_internal.ml write_internal.ml symbols.ml conversion.ml internal.ml parser.ml reduction.ml revEng.ml > r.ml

standard.ml: standard.R
	R --silent --vanilla --slave < standard.R > standard.ml

r_stubs.o: r_stubs.c
	ocamlopt -ccopt -Wall $(COMPFLAGS) -ccopt -fPIC -c $<

libr_stubs.a: r_stubs.o
	ar rcs libr_stubs.a r_stubs.o

dllr_stubs.so: libr_stubs.a r_stubs.o
	ocamlmklib -o r_stubs r_stubs.o

clean:
	rm -f standard.ml r.ml
	rm -f *.o *.so *.a *.cmi *.cmo *.cmx *.cma *.cmxa

test: build
	ocaml -init ocamlinit

install:
	cp META *.a *.cm[ai] *.cmxa /usr/lib/ocaml/R/
	cp dllr_stubs.so /usr/lib/ocaml/stublibs/
