.SUFFIXES: .o .lsp

default:
	@echo "Targets:"
	@echo "   clean - remove build droppings"
	@echo "   sbcl  - build using SBCL"
	@echo "   clisp - build using CLISP"
	@echo "        "

## Workspace cleanup

clean:
	(cd lib; make clean)
	rm -f *.o *.fasl kclcmplr *~

cleanall:
	(cd lib; make cleanall)
	rm -f *.o *.fasl kclcmplr saved_kcls kcls cls

## C compilation

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


## Lisp targets and compilation

sbcl : 


clisp : 


## Debugging/Building


sbcl-debug :


clisp-debug : 



