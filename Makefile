.SUFFIXES: .o .lsp

#
# Modify according to your system's needs for dynamic loading
#
CFLAGS = -O -G 0 -DINTPTR
#
# AKCL directory and executable
#
AKCLDIR=/WAR/usr/users/mikem/Software/Kcl/akcl
AKCL=${AKCLDIR}/unixport/saved_kcl ${AKCLDIR}/unixport/
#
# EXCL (Allegro) batch command
#
EXCL=/CHAGRIN/usr/users/luke/LS/KCL/ACLS/cl -batch
#
# Directory for Examples and Data -- MUST end in a / if not null
# For now, you have to install the Examples and Data directories by hand.
#
LSLIB=/usr/statlocal/lib/xlispstat/
#
# Directory for saved_kcls binary
#
KCLSDIR=/CHAGRIN/usr/users/luke/LS/KCL/KCLS/

kcl:	Makefile saved_kcls
	echo "#!/bin/csh -f" > kcls
	echo "set KCLSDIR=${KCLSDIR}" >> kcls
	echo "setenv LSLIB ${LSLIB}" >> kcls
	echo '$${KCLSDIR}saved_kcls $${KCLSDIR}' >> kcls
	chmod +x kcls

excl:	Makefile lib/clib.a lib/exclglue.o
	echo '(load "defsys") (ls::compile-stats)' | ${EXCL}
	sed "s?./?${LSLIB}?" makesys.excl.dist > makesys.excl
	cat makesys.excl | ${EXCL}
	
saved_kcls: lib/clib.a
	echo '(load "defsys") (ls::compile-stats)' | ${AKCL}
	cat makesys.kcl | ${AKCL}
	
lib/clib.a:
	(cd lib; make CFLAGS="${CFLAGS}")

lib/exclglue.o:
	(cd lib; make CFLAGS="${CFLAGS}" exclglue.o)

clean:
	(cd lib; make clean)
	rm -f *.o *.fasl kclcmplr

cleanall:
	(cd lib; make cleanall)
	rm -f *.o *.fasl kclcmplr saved_kcls kcls cls

#
# This lets you use 'make objects' to make .o files for changed .lsp
# files for kcl. If you use this approach, use 'cat makesys.kcl | kcl'
# to make the executable.
#

OBJECTS=bayes.o compound.o dists.o fastmap.o kclglue.o \
	kclpatch.o ladata.o linalg.o lsbasics.o lsfloat.o lsmath.o \
	lsobjects.o lstoplevel.o matrices.o maximize.o nonlin.o \
	regression.o statistics.o

.lsp.o:
	./kclcmplr ./ $*.lsp $*.lsp S1000

kclcmplr: Makefile defsys.lsp lsmacros.lsp lspackages.lsp
	rm -f cmpinclude.h
	ln -s ${AKCLDIR}/h/cmpinclude.h cmpinclude.h
	echo '(load "defsys.lsp") (load "kclpatch.lsp") (load "lsobjects.lsp") (load "lsbasics.lsp") (load "ladata.lsp") (si:save-system "kclcmplr")' | ${AKCL}


objects: kclcmplr ${OBJECTS}
