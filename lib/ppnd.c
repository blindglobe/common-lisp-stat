#include "xmath.h"

#define zero   0.0 
#define half   0.5 
#define one    1.0 
#define split  0.42e0 
#define a0     2.50662823884e0 
#define a1   -18.61500062529e0 
#define a2    41.39119773534e0 
#define a3   -25.44106049637e0 
#define b1    -8.47351093090e0 
#define b2    23.08336743743e0 
#define b3   -21.06224101826e0 
#define b4     3.13082909833e0 

#define c0    -2.78718931138e0 
#define c1    -2.29796479134e0 
#define c2     4.85014127135e0 
#define c3     2.32121276858e0 
#define d1     3.54388924762e0 
#define d2     1.63706781897e0 

/*
c
c   Algorithm as 111 Applied statistics (1977), vol 26 no 1 page 121
c   Produces normal deviate corresponding to lower tail area of p
c   the hash sums are the sums of the moduli of the coefficients
c   they nave no inherent meanings but are incuded for use in
c   checking transcriptions.  Functions abs,alog and sqrt are used.
c

derived from AS111 fortran version
*/

double ppnd(p, ifault)
	double p;
	int *ifault;
{
  double q,r,ppn;

  *ifault = 0;
  q = p - half;
  if( fabs(q) <= split) {
    r = q*q;
    ppn = q *  (((a3 * r + a2) * r + a1) * r + a0) 
            / ((((b4 * r + b3) * r + b2) * r + b1) * r + one);
  }
  else {
    r = p;
    if(q > zero) r = one - p;
    if(r <= zero) {
      *ifault = 1;
      return(0.0);
    }
    r = sqrt(-log(r));
    ppn = (((c3*r+c2)*r + c1) * r + c0) / ((d2 * r + d1) * r + one);
    if( q < zero) ppn = -ppn;
  }
  return(ppn);
}
