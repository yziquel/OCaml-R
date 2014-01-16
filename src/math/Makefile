RINCLUDES=-I . -I /usr/share/R/include
INCLUDES=       -I +ocamldoc -I `ocamlc -where`/caml $(RINCLUDES)

COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES) -cclib -lRmath
LINKFLAGS_BYTE=$(INCLUDES) -cclib -lRmath

all: byte native

byte: rmath.cma
native: rmath.cmxa rmath.cmxs

rmath.cma: dllrmath_stubs.so rmath.cmo
	ocamlc -verbose -a -dllib dllrmath_stubs.so -dllib libRmath.so -o rmath.cma rmath.cmo

rmath.cmxa: dllrmath_stubs.so rmath.cmx
	ocamlopt -verbose -a -cclib -lrmath_stubs -cclib -lRmath -o rmath.cmxa rmath.cmx

rmath.cmxs: dllrmath_stubs.so rmath.cmx rmath_stubs.o
	ocamlopt -verbose -shared rmath_stubs.o -o rmath.cmxs rmath.cmx

rmath.cmi: rmath.mli
	ocamlfind ocamlc -verbose -c rmath.mli

rmath.cmo: rmath.ml rmath.cmi
	ocamlfind ocamlc -verbose -c rmath.ml

rmath.cmx: rmath.ml rmath.cmi
	ocamlfind ocamlopt -verbose -c rmath.ml

rmath_stubs.o: rmath_stubs.c
	ocamlopt -verbose -ccopt -Werror $(COMPFLAGS) -ccopt -fPIC -c $<

librmath_stubs.a: rmath_stubs.o
	ar rcs librmath_stubs.a rmath_stubs.o

dllrmath_stubs.so: librmath_stubs.a rmath_stubs.o
	ocamlmklib -verbose -o rmath_stubs rmath_stubs.o

clean:
	rm -f *.o *.so *.a *.cmi *.cmo *.cmx *.cma *.cmxa *.cmxs
