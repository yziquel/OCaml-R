RPACKAGE=rmethods

all: _build/$(RPACKAGE).otarget byte native

clean:
	rm -f *.o *.cmo *.cmx
	ocamlbuild -clean

byte: _build/$(RPACKAGE).byte.otarget
native: _build/$(RPACKAGE).native.otarget

_build/$(RPACKAGE).otarget: $(RPACKAGE).itarget
	cp ../r.cmi .
	ocamlbuild -no-hygiene $(RPACKAGE).otarget
	r -f r.cmi

_build/$(RPACKAGE).byte.otarget: $(RPACKAGE).byte.itarget
	cp ../r.cmi .
	ocamlbuild -no-hygiene $(RPACKAGE).byte.otarget
	rm -f r.cmi

_build/$(RPACKAGE).native.otarget: $(RPACKAGE).native.itarget
	cp ../r.cmi .
	ocamlbuild -no-hygiene $(RPACKAGE).native.otarget
	rm -f r.cmi
