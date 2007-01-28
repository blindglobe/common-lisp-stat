#############################################################
## Uncomment one of the following two groups for a generic
## application or one compiled to use 68020/68881 intructions.
##
## Generic application
##
CC = C
##
## MC68020/MC68881 application
##
#CC = C -mc68881 -elems881 -mc68020
#############################################################
#############################################################
#   File:       clib.make
#   Target:     clib
#   Sources:    betabase.c
#               bivnor.c
#               cbayes.c
#               cdists.c
#               cfft.c
#               cholesky.c
#               clinalg.c
#               complex.c
#               derivatives.c
#               functions.c
#               gamln.c
#               gammabase.c
#               kernel.c
#               linalgdata.c
#               lowess.c
#               ludecomp.c
#               makerotation.c
#               minimize.c
#               nor.c
#               ppnd.c
#               qrdecomp.c
#               rcondest.c
#               splines.c
#               studentbase.c
#               svdecomp.c
#               utils.c
#               mclglue.c
#   Created:    Friday, August 17, 1990 8:16:25 AM


OBJECTS = 6
		betabase.c.o 6
		bivnor.c.o 6
		cbayes.c.o 6
		cdists.c.o 6
		cfft.c.o 6
		cholesky.c.o 6
		clinalg.c.o 6
		complex.c.o 6
		derivatives.c.o 6
		eigen.c.o 6
		functions.c.o 6
		gamln.c.o 6
		gammabase.c.o 6
		kernel.c.o 6
		linalgdata.c.o 6
		lowess.c.o 6
		ludecomp.c.o 6
		makerotation.c.o 6
		minimize.c.o 6
		nor.c.o 6
		ppnd.c.o 6
		qrdecomp.c.o 6
		rcondest.c.o 6
		splines.c.o 6
		studentbase.c.o 6
		svdecomp.c.o 6


betabase.c.o D clib.make betabase.c
	 {CC}  -s BETABASE betabase.c
bivnor.c.o D clib.make bivnor.c
	 {CC}  -s BIVNOR bivnor.c
cbayes.c.o D clib.make cbayes.c
	 {CC}  -s BAYES cbayes.c
cdists.c.o D clib.make cdists.c
	 {CC}  -s CDISTS cdists.c
cfft.c.o D clib.make cfft.c
	 {CC}  -s CFFT cfft.c
cholesky.c.o D clib.make cholesky.c
	 {CC}  -s CHOLESKY cholesky.c
clinalg.c.o D clib.make clinalg.c
	 {CC}  -s LINALG clinalg.c
complex.c.o D clib.make complex.c
	 {CC}  -s COMPLEX complex.c
derivatives.c.o D clib.make derivatives.c
	 {CC}  -s BAYES derivatives.c
eigen.c.o D clib.make eigen.c
	 {CC}  -s EIGEN eigen.c
functions.c.o D clib.make functions.c
	 {CC}  -s BAYES functions.c
gamln.c.o D clib.make gamln.c
	 {CC}  -s GAMMA gamln.c
gammabase.c.o D clib.make gammabase.c
	 {CC}  -s GAMMA gammabase.c
kernel.c.o D clib.make kernel.c
	 {CC}  -s SMOOTH kernel.c
linalgdata.c.o D clib.make linalgdata.c
	 {CC}  -s LINALG linalgdata.c
lowess.c.o D clib.make lowess.c
	 {CC}  -s SMOOTH lowess.c
ludecomp.c.o D clib.make ludecomp.c
	 {CC}  -s LUDECOMP ludecomp.c
makerotation.c.o D clib.make makerotation.c
	 {CC}  -s MAKEROT makerotation.c
minimize.c.o D clib.make minimize.c
	 {CC}  -s BAYES minimize.c
nor.c.o D clib.make nor.c
	 {CC}  -s NORMAL nor.c
ppnd.c.o D clib.make ppnd.c
	 {CC}  -s NORMAL ppnd.c
qrdecomp.c.o D clib.make qrdecomp.c
	 {CC} -d SCALE=XSSCALE -s QRDECOMP qrdecomp.c    # needed besause of name conflict with SCALE???
rcondest.c.o D clib.make rcondest.c
	 {CC}  -s RCONDEST rcondest.c
splines.c.o D clib.make splines.c
	 {CC}  -s SMOOTH splines.c
studentbase.c.o D clib.make studentbase.c
	 {CC}  -s STUDENT studentbase.c
svdecomp.c.o D clib.make svdecomp.c
	 {CC}  -s SVDECOMP svdecomp.c
mclglue.c.o D clib.make mclglue.c
	 {CC}  -s LSGLUE mclglue.c

clib.o DD clib.make {OBJECTS} mclglue.c.o ccldists.c.o
	lib -o clib.o {OBJECTS}
	
clib DD clib.make clib.o mclglue.c.o

