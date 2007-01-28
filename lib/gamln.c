#include "xmath.h"

/*
log gamma function from Numerical Recipes
*/

static double cof[6] = {
   76.18009173,
  -86.50532033,
   24.01409822,
   -1.231739516,
    0.120858003e-2,
   -0.536382e-5
};

double gamma(xx)
	double xx;
{
  double x, tmp, ser;
  int j;
  
  if (xx < 1.0) return(gamma(1.0 + xx) - log(xx));
  else {
    x = xx - 1.0;
    tmp = x + 5.5;
    tmp -= (x + 0.5) * log(tmp);
    ser = 1.0;
    for (j = 0; j < 6; j++) {
      x += 1.0;
      ser += cof[j] / x;
    }
    return(-tmp + log(2.50662827465 * ser));
  }
}
 
