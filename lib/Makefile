CDISTOBJS = betabase.o bivnor.o cdists.o gamln.o gammabase.o nor.o ppnd.o \
	studentbase.o

CLINALGOBJS = cholesky.o linalgdata.o ludecomp.o complex.o svdecomp.o \
	qrdecomp.o cfft.o clinalg.o rcondest.o kernel.o lowess.o splines.o \
	makerotation.o eigen.o

CBAYESOBJS = cbayes.o functions.o derivatives.o minimize.o

CFLAGS = -O -G 0 -DINTPTR

kcl: clib.a

excl: clib.a exclglue.o

clib.a: ${CDISTOBJS} ${CLINALGOBJS} ${CBAYESOBJS}
	ar rcv clib.a ${CDISTOBJS} ${CLINALGOBJS} ${CBAYESOBJS}
	ranlib clib.a

clean:
	rm -f ${CDISTOBJS} ${CLINALGOBJS} ${CBAYESOBJS} *~

cleanall:
	make clean
	rm -f clib.a exclglue.o
