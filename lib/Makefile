LISPSTAT-SO = liblispstat.so

CDISTOBJS = betabase.o bivnor.o cdists.o gamln.o gammabase.o nor.o ppnd.o \
	studentbase.o

CLINALGOBJS = cholesky.o linalgdata.o ludecomp.o complex.o svdecomp.o \
	qrdecomp.o cfft.o clinalg.o rcondest.o kernel.o lowess.o splines.o \
	makerotation.o eigen.o

CBAYESOBJS = cbayes.o functions.o derivatives.o minimize.o

CGLUE = cffi-glue.o

CFLAGS = -g -DINTPTR -fPIC -Wall

cffi : $(LISPSTAT-SO)

sbcl : clib.a

kcl: clib.a

excl: clib.a exclglue.o

clib.a: ${CDISTOBJS} ${CLINALGOBJS} ${CBAYESOBJS} $(CGLUE)
	ar rcv clib.a ${CDISTOBJS} ${CLINALGOBJS} ${CBAYESOBJS} $(CGLUE)
	ranlib clib.a

$(LISPSTAT-SO) :${CDISTOBJS} ${CLINALGOBJS} ${CBAYESOBJS} $(CGLUE)
	gcc -shared -o $(LISPSTAT-SO)  ${CDISTOBJS} ${CLINALGOBJS} ${CBAYESOBJS}  $(CGLUE)

clean:
	rm -f ${CDISTOBJS} ${CLINALGOBJS} ${CBAYESOBJS} $(CGLUE) *~

cleanall:
	make clean
	rm -f clib.a exclglue.o
