RPACKAGE=rstats

all: _build/$(RPACKAGE).otarget byte native

clean:
	rm -f *.o *.cmo *.cmx r.cmi
	ocamlbuild -clean

byte: _build/$(RPACKAGE).byte.otarget $(RPACKAGE)Hook.cmo
native: _build/$(RPACKAGE).native.otarget $(RPACKAGE)Hook.cmx

$(RPACKAGE)Hook.cmo: r.cmi _build/$(RPACKAGE).byte.otarget $(RPACKAGE)Hook.ml
	ocamlfind ocamlc -c $(RPACKAGE)Hook.ml
	rm -f $(RPACKAGE)Hook.cmi

$(RPACKAGE)Hook.cmx: r.cmi _build/$(RPACKAGE).native.otarget $(RPACKAGE)Hook.ml
	ocamlfind ocamlopt -c $(RPACKAGE)Hook.ml
	rm -f $(RPACKAGE)Hook.cmi

_build/$(RPACKAGE).otarget: r.cmi $(RPACKAGE).itarget
	ocamlbuild -no-hygiene $(RPACKAGE).otarget

_build/$(RPACKAGE).byte.otarget: r.cmi $(RPACKAGE).byte.itarget
	ocamlbuild -no-hygiene $(RPACKAGE).byte.otarget

_build/$(RPACKAGE).native.otarget: r.cmi $(RPACKAGE).native.itarget
	ocamlbuild -no-hygiene $(RPACKAGE).native.otarget

r.cmi: ../r.cmi
	cp ../r.cmi r.cmi
