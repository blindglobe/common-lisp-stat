/* I hate C programming... */

typedef struct {
  double real, imag;
} Complex;
 
#ifndef PI
#define PI 3.141592653589793
#endif /* PI */

extern Complex makecomplex();
extern Complex cart2complex();
extern Complex polar2complex(double, double);
/* extern Complex csqrt(), cexp(), clog(), cexpt(), csin(), ccos(), ctan(); 
   extern Complex casin(), cacos(), catan(); */
extern Complex cadd(), csub(), cmul(), cdiv();
extern /* static */ double phase(Complex);
extern double modulus(Complex);
