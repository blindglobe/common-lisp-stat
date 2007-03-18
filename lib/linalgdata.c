/* linalgdata - allocation support for basic linear algebra routines.  */
/* Copyright (c) 1990, by Luke Tierney                                 */

#include "linalg.h"

#ifdef INTPTR
typedef int PTR;
#else
typedef char *PTR;
#endif

extern PTR la_allocate(int, int);
extern void la_free_alloc(PTR);
extern void xlfail(char *);


/************************************************************************/
/**                                                                    **/
/**                 Storage Allocation Functions                       **/
/**                                                                    **/
/************************************************************************/

static char 
*allocate(unsigned n, unsigned m)
{
  char *p = (char *) la_allocate(n, m);
  if (p == nil) xlfail("allocation failed");
  return(p);
}

static void
free_alloc(char *p)
{
  if (p != nil) la_free_alloc((PTR) p);
}

IVector
ivector(unsigned n)
{
  return((IVector) allocate(n, sizeof(int)));
}

double
*rvector(unsigned n)
{
  return((double *) allocate(n, sizeof(double)));
}

CVector cvector(n)
	unsigned n;
{
  return((CVector) allocate(n, sizeof(Complex)));
}

void
free_vector(double *v)
{
  free_alloc((char *)v);
}

IMatrix imatrix(unsigned n, unsigned m)
{
  int i;
  IMatrix mat = (IMatrix) allocate(n, sizeof(IVector));
  for (i = 0; i < n; i++) mat[i] = (IVector) allocate(m, sizeof(int));
  return(mat);
}

RMatrix
rmatrix(unsigned n, unsigned m)
{
  int i;
  RMatrix mat = (RMatrix) allocate(n, sizeof(RVector));
  for (i = 0; i < n; i++) mat[i] = (RVector) allocate(m, sizeof(double));
  return(mat);
}

CMatrix
cmatrix(unsigned n, unsigned m)
{
  int i;
  CMatrix mat = (CMatrix) allocate(n, sizeof(CVector));
  for (i = 0; i < n; i++) mat[i] = (CVector) allocate(m, sizeof(Complex));
  return(mat);
}

void
free_matrix(double **mat, int n) /* Matrix?? Not RMatrix?? */
{
  int i;
  
  if (mat != nil) for (i = 0; i < n; i++) free_alloc((char *)mat[i]);
  free_alloc((char *)mat);
}
