typedef struct {
  double real, imag;
} Complex;
 
#ifndef PI
#define PI 3.141592653589793
#endif PI

extern Complex makecomplex();
extern Complex cart2complex(), polar2complex();
extern Complex csqrt(), cexp(), clog(), cexpt(), csin(), ccos(), ctan();
extern Complex casin(), cacos(), catan();
extern Complex cadd(), csub(), cmul(), cdiv();
extern double phase(), modulus();
