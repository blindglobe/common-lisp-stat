.SUFFIXES: .o .lsp

default:
	@echo "Targets:"
	@echo "   clean - remove build droppings"
	@echo "   sbcl  - build using SBCL"
	@echo "   clisp - build using CLISP"
	@echo "        "

#
# Modify according to your system's needs for dynamic loading
#
CFLAGS = -O -G 0 -DINTPTR

# Directory for Examples and Data -- MUST end in a / if not null
# For now, you have to install the Examples and Data directories by hand.
#
LSLIB=/usr/statlocal/lib/xlispstat/

lib/clib.a:
	(cd lib; make CFLAGS="${CFLAGS}")

lib/exclglue.o:
	(cd lib; make CFLAGS="${CFLAGS}" exclglue.o)

clean:
	(cd lib; make clean)
	rm -f *.o *.fasl kclcmplr *~

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

