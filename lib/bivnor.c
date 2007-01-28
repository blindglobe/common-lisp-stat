#include "xmath.h"

#define twopi 6.283195307179587
#define con (twopi / 2.0) * 10.0e-10

double bivnor(ah, ak, r)
     double ah, ak, r;
{
  /*
    based on alg 4628 comm. acm oct 73
    gives the probability that a bivariate normal exceeds (ah,ak).
    gh and gk are .5 times the right tail areas of ah, ak under a n(0,1)

    Tranlated from FORTRAN to ratfor using struct; from ratfor to C by hand.
  */
  double a2, ap, b, cn, conex, ex, g2, gh, gk, gw, h2, h4, rr, s1, s2, 
    sgn, sn, sp, sqr, t, temp, w2, wh, wk;
  int is;

  temp = -ah;
  normbase(&temp, &gh);
  gh = gh / 2.0;
  temp = -ak;
  normbase(&temp, &gk);
  gk = gk / 2.0;

  b = 0;

  if (r==0)
    b = 4*gh*gk;
  else {
    rr = 1-r*r;
    if (rr<0)
      return(-1.0);
    if (rr!=0) {
      sqr = sqrt(rr);
      if (ah!=0) {
	b = gh;
	if (ah*ak<0)
	  b = b-.5;
	else if (ah*ak==0)
	  goto label10;
      }
      else if (ak==0) {
	b = atan(r/sqr)/twopi+.25;
	goto label50;
      }
      b = b+gk;
      if (ah==0)
	goto label20;
    label10:  
      wh = -ah;
      wk = (ak/ah-r)/sqr;
      gw = 2*gh;
      is = -1;
      goto label30;
    label20:  
      do {
	wh = -ak;
	wk = (ah/ak-r)/sqr;
	gw = 2*gk;
	is = 1;
      label30:  
	sgn = -1;
	t = 0;
	if (wk!=0) {
	  if (fabs(wk)>=1)
	    if (fabs(wk)==1) {
	      t = wk*gw*(1-gw)/2;
	      goto label40;
	    }
	    else {
	      sgn = -sgn;
	      wh = wh*wk;
	      normbase(&wh, &g2);
	      wk = 1/wk;
	      if (wk<0)
		b = b+.5;
	      b = b-(gw+g2)/2+gw*g2;
	    }
	  h2 = wh*wh;
	  a2 = wk*wk;
	  h4 = h2*.5;
	  ex = 0;
	  if (h4<150.0)
	    ex = exp(-h4);
	  w2 = h4*ex;
	  ap = 1;
	  s2 = ap-ex;
	  sp = ap;
	  s1 = 0;
	  sn = s1;
	  conex = fabs(con/wk);
	  do {
	    cn = ap*s2/(sn+sp);
	    s1 = s1+cn;
	    if (fabs(cn)<=conex)
	      break;
	    sn = sp;
	    sp = sp+1;
	    s2 = s2-w2;
	    w2 = w2*h4/sp;
	    ap = -ap*a2;
	  } while (1);
	  t = (atan(wk)-wk*s1)/twopi;
	label40:  
	  b = b+sgn*t;
	}
	if (is>=0)
	  break;
      } while(ak!=0);
    }
    else if (r>=0)
      if (ah>=ak)
	b = 2*gh;
      else
	b = 2*gk;
    else if (ah+ak<0)
      b = 2*(gh+gk)-1;
  }
 label50: 
  if (b<0)
    b = 0;
  if (b>1)
    b = 1;

  return(b);
}
