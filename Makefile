#################################################################################
#                OCaml-R                                                        #
#                                                                               #
#    Copyright (C) 2008 Institut National de Recherche en Informatique et       #
#    en Automatique. All rights reserved.                                       #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation; either version 2 of the         #
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

include master.Makefile

# Compilation
#############

first: src
all:
	cd src && $(MAKE) all

src: dummy
	cd src && $(MAKE)

re : depend clean all


# Documentation :
#################
doc: dummy
	cd src && $(MAKE) doc

# myself

master.Makefile: master.Makefile.in config.status
	./config.status

config.status: configure master.Makefile.in
	./config.status --recheck

configure: configure.in
	autoconf

# headers :
###########
HEADFILES= configure.in configure \
	master.Makefile.in Makefile \
	src/*.ml src/*.mli src/*.mll src/*.mly src/*.in src/*.c \
	src/Makefile checkocaml.ml
headers: dummy
	echo $(HEADFILES)
	headache -h header -c .headache_config `ls $(HEADFILES)`

noheaders: dummy
	headache -r -c .headache_config `ls $(HEADFILES)`

# backup, clean and depend :
############################

distclean: clean
	cd src && $(MAKE) distclean
	$(RM) config.cache config.log config.status configure.lineno config.status.lineno
	$(RM) master.Makefile
	$(RM) config_check.log ocaml_config.sh
	$(RM) autom4te.cache
#	cd utils && $(MAKE) distclean
#	cd doc && $(MAKE) distclean

clean:: dummy
	$(RM) *~ \#*\#
	cd src && $(MAKE) clean
#	cd utils && $(MAKE) clean
#	cd doc && $(MAKE) clean


depend: dummy
	cd src && $(MAKE) depend

dummy:

#################
# code count
#################
codecount:
	@echo `cat src/*.ml src/*.mli | wc -l` lines


#################
# installation
#################

install: dummy
	cd src && $(MAKE) install


###########################
# additional dependencies
###########################

# DO NOT DELETE
