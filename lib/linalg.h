# include "xmath.h"
# include "xlisp.h"
# include "complex.h"

extern void *calloc(); /*conflict?  was (char *) */
extern double macheps();

#define nil 0L

typedef char **Matrix, *Vector;
typedef int **IMatrix, *IVector;
typedef double **RMatrix, *RVector;
typedef Complex **CMatrix, *CVector;

#define IN 0
#define RE 1
#define CX 2

/* external functions */
extern IVector ivector();
extern RVector rvector();
extern CVector cvector();
extern IMatrix imatrix();
extern RMatrix rmatrix();
extern CMatrix cmatrix();

extern void free_vector(double *);
