RLIBDIR=/usr/lib/R/lib
RINCLUDES=-I . -I /usr/share/R/include
INCLUDES=       -I +ocamldoc -I `ocamlc -where`/caml $(RINCLUDES)

COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES) -ccopt -L$(RLIBDIR) -cclib -lR
LINKFLAGS_BYTE=$(INCLUDES) -ccopt -L$(RLIBDIR)  -cclib -lR

all: build

build: r.cma r.cmxa oCamlR.cma oCamlR.cmxa

r.cma: dllr_stubs.so camlobjs
	ocamlc -a -dllpath /usr/lib/R/lib -dllib dllr_stubs.so -dllib libR.so -o r.cma r_Env.cmo standard.cmo initialisation.cmo sexptype.cmo sexprec.cmo data.cmo allocation.cmo read_internal.cmo write_internal.cmo symbols.cmo conversion.cmo internal.cmo reduction.cmo parser.cmo revEng.cmo r.cmo

r.cmxa: dllr_stubs.so camlobjs
	ocamlopt -a -ccopt -L/usr/lib/R/lib -cclib -lr_stubs -cclib -lR -o r.cmxa r_Env.cmx standard.cmx initialisation.cmx sexptype.cmx sexprec.cmx data.cmx allocation.cmx read_internal.cmx write_internal.cmx symbols.cmx conversion.cmx internal.cmx reduction.cmx parser.cmx revEng.cmx r.cmx

oCamlR.cma: camlobjs
	ocamlc -a -o oCamlR.cma oCamlR.cmo

oCamlR.cmxa: camlobjs
	ocamlopt -a -o oCamlR.cmxa oCamlR.cmx

camlobjs: standard.ml
	ocamlbuild -classic-display -no-hygiene r_Env.cmo standard.cmo initialisation.cmo sexptype.cmo sexprec.cmo data.cmo allocation.cmo read_internal.cmo write_internal.cmo symbols.cmo conversion.cmo internal.cmo reduction.cmo parser.cmo revEng.cmo r.cmo r_Env.cmx standard.cmx initialisation.cmx sexptype.cmx sexprec.cmx data.cmx allocation.cmx read_internal.cmx write_internal.cmx symbols.cmx conversion.cmx internal.cmx reduction.cmx parser.cmx revEng.cmx r.cmx r.cmi oCamlR.cmo oCamlR.cmx oCamlR.cmi sexptype.cmi data.cmi
	cp _build/*.cmo .
	cp _build/*.cmx .
	cp _build/r.cmi .
	cp _build/oCamlR.cmi .
	cp _build/sexptype.cmi .
	cp _build/data.cmi .
	cp _build/*.o .

standard.ml: standard.R
	R --silent --vanilla --slave < standard.R > standard.ml

r_stubs.o: r_stubs.c
	ocamlopt -ccopt -Wall $(COMPFLAGS) -ccopt -fPIC -c $<

libr_stubs.a: r_stubs.o
	ar rcs libr_stubs.a r_stubs.o

dllr_stubs.so: libr_stubs.a r_stubs.o
	ocamlmklib -o r_stubs r_stubs.o

debug:
	ocamlbuild -tag debug r.cma

clean:
	rm -f standard.ml
	rm -f *.o *.so *.a *.cmi *.cmo *.cmx *.cma *.cmxa
	ocamlbuild -clean

test: build
	ocaml -init ocamlinit
