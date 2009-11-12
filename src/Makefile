#################################################################################
#                OCaml-R                                                        #
#                                                                               #
#    Copyright (C) 2008-2009 Institut National de Recherche en                  #
#    Informatique et en Automatique. All rights reserved.                       #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation; either version 3 of the         #
#    License, or  any later version.                                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU General Public                  #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

include ../master.Makefile

RLIBDIR=/usr/lib/R/lib
RINCLUDES=-I . -I /usr/share/R/include
INCLUDES=	-I +ocamldoc -I `ocamlc -where`/caml $(RINCLUDES)

COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES) -ccopt -L$(RLIBDIR) -cclib -lR
LINKFLAGS_BYTE=$(INCLUDES) -ccopt -L$(RLIBDIR)  -cclib -lR
LINKFLAGS_MATH=$(INCLUDES) -ccopt -L$(RLIBDIR) -cclib -lRmath
LINKFLAGS_MATH_BYTE=$(INCLUDES) -ccopt -L$(RLIBDIR)  -cclib -lRmath

#OCAMLPP=-pp "camlp4o "

# Compilation
#############

LIB_CMOFILES=\
	rdevices.cmo \
	r.cmo

LIB_CMXFILES=$(LIB_CMOFILES:.cmo=.cmx)
LIB_CMIFILES= $(LIB_CMOFILES:.cmo=.cmi)

LIB_COBJ_FILES=r_base.o\
	r_data.o \
#	r_devices.o

LIBMATH_CMOFILES=\
	rmath.cmo

PA_R=pa_ocamlr.cmo

LIBMATH_CMXFILES=$(LIBMATH_CMOFILES:.cmo=.cmx)
LIBMATH_CMIFILES= $(LIBMATH_CMOFILES:.cmo=.cmi)

LIBMATH_COBJ_FILES=r_math.o

############
opt: lib
byte: lib_byte $(PA_R)
lib: $(LIB) $(LIB_CMI) $(LIBINTERP) $(LIBINTERP_CMI) $(LIBMATH) $(LIBMATH_CMI)
lib_byte: $(LIB_BYTE) $(LIB_CMI) $(LIBINTERP_BYTE) $(LIBINTERP_CMI) $(LIBMATH_BYTE) $(LIBMATH_CMI)

all: byte opt

$(LIB_BYTE): dllr_base_stubs.so dllr_data_stubs.so $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLC) -a -dllpath /usr/lib/R/lib \
	-dllib dllr_base_stubs.so \
	-dllib dllr_data_stubs.so \
	-dllib libR.so -o $@ \
	$(LIB_CMOFILES)

$(LIB): dllr_base_stubs.so dllr_data_stubs.so $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLOPT) -a -ccopt -L/usr/lib/R/lib \
	-cclib -lr_base_stubs -cclib -lr_data_stubs \
	-cclib -lR \
	-o $@ $(LIB_CMXFILES)

$(LIBINTERP_BYTE): oCamlR.cmi oCamlR.cmo
	$(OCAMLC) -a -o $@ oCamlR.cmo

$(LIBINTERP): oCamlR.cmi oCamlR.cmx
	$(OCAMLOPT) -a -o $@ oCamlR.cmx

$(LIBMATH_BYTE): dllr_math_stubs.so $(LIBMATH_CMIFILES) $(LIBMATH_CMOFILES)
	$(OCAMLC) -a -dllib dllr_math_stubs.so -dllib libRmath.so \
	-o $@ $(LIBMATH_CMOFILES)

$(LIBMATH): dllr_math_stubs.so $(LIBMATH_CMIFILES) $(LIBMATH_CMXFILES)
	$(OCAMLOPT) -a -cclib -lr_math_stubs -cclib -lRmath \
	-o $@ $(LIBMATH_CMXFILES)

dllr_base_stubs.so: libr_base_stubs.a r_base.o
	$(OCAMLMKLIB) -o r_base_stubs r_base.o

dllr_data_stubs.so: libr_data_stubs.a r_data.o
	$(OCAMLMKLIB) -o r_data_stubs r_data.o

dllr_math_stubs.so: libr_math_stubs.a r_math.o
	$(OCAMLMKLIB) -o r_math_stubs r_math.o

libr_base_stubs.a: r_base.o
	ar rcs libr_base_stubs.a r_base.o

libr_data_stubs.a: r_data.o
	ar rcs libr_data_stubs.a r_data.o

libr_math_stubs.a: r_math.o
	ar rcs libr_math_stubs.a r_math.o

$(PA_R): pa_ocamlr.ml
	$(OCAMLC) -I +camlp4 -I +camlp4/Camlp4Parsers -c -pp camlp4of.opt $<

r.ml.part1: rstdenv.R
	$(R) --silent --vanilla --slave < rstdenv.R > r.ml.part1

r.ml: r.ml.part1 r.ml.part2
	cat r.ml.part1 r.ml.part2 > r.ml

test.x: $(LIB) $(LIBMATH) test.cmx
	$(OCAMLOPT) -verbose -o $@ unix.cmxa $(LIBMATH) $(LIB) test.cmx
test.byte: $(LIB_BYTE) $(LIBMATH_BYTE) test.cmo
	$(OCAMLC) -verbose -o $@ unix.cma $(LIB_BYTE) $(LIBMATH_BYTE) test.cmo

# Documentation :
#################
dump.odoc: *.mli
	$(OCAMLDOCOPT) $(INCLUDES) -keep-code $(OCAMLPP) $(COMPFLAGS) -sort -dump $@ \
	`ls $^ | grep -v example.ml | grep -v example2.ml | grep -v test`

doc: all dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLDOCOPT) $(OCAMLPP) $(COMPFLAGS) \
	-html \
	-load dump.odoc \
	-d ocamldoc -v -t "OCaml-R"

dot: dep.dot
dep.dot: dump.odoc
	$(OCAMLDOCOPT) -load $< -o $@ -dot -dot-reduce
dep.ps:dep.dot
	dot -Tps -o $@ $<

# backup, clean and depend :
############################

GENERATED_FILES=rstdenv.ml

cleanocamldoc:
	$(RM) -r ocamldoc

distclean: clean
	$(MAKE) cleanocamldoc
	$(RM) dump.odoc

clean:: dummy
	$(RM) *~ \#*\#
	$(RM) *.a *.so
	$(RM) *~ \#*\# *.cmo *.cmi *.cmx *.a *.o *.cma *.cmxa *.annot
	$(RM) test.x
	$(RM) $(GENERATED_FILES)

.depend depend:
	$(RM) .depend
	$(OCAMLDEP) $(OCAMLPP) `ls *.ml *.mli | grep -v test_pa | grep -v pa_ocaml` > .depend

alldepend: $(GENERATED_FILES)
	$(MAKE) depend
dummy:

include .depend

#################
# Installation
#################
install: installlibs
# installlibs installplugins


installlibs: dummy
	$(MKDIR) $(LIBDIR)
	@for i in $(LIB) $(LIB_BYTE) $(LIB_CMI) $(LIBMATH) $(LIBMATH_BYTE) $(LIBMATH_CMI) \
	$(LIB:.cmxa=.a) $(LIBMATH:.cmxa=.a) libr*.a dllr*.so ; do \
	if test -f $$i; then \
		$(CP) $$i $(LIBDIR)/; echo $(LIBDIR)/`basename $$i` installed; \
	else \
		echo $$i NOT installed ; \
	fi; done

###########################
# additional dependencies
###########################

