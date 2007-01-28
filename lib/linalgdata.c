/* linalgdata - allocation support for basic linear algebra routines.  */
/* Copyright (c) 1990, by Luke Tierney                                 */

#include "linalg.h"

#ifdef INTPTR
typedef int PTR;
#else
typedef char *PTR;
#endif

extern PTR la_allocate();

/************************************************************************/
/**                                                                    **/
/**                 Storage Allocation Functions                       **/
/**                                                                    **/
/************************************************************************/

static char *allocate(n, m)
	unsigned n, m;
{
  char *p = (char *) la_allocate(n, m);
  if (p == nil) xlfail("allocation failed");
  return(p);
}

static free_alloc(p)
	char *p;
{
  if (p != nil) la_free_alloc((PTR) p);
}

IVector ivector(n)
	unsigned n;
{
  return((IVector) allocate(n, sizeof(int)));
}

RVector rvector(n)
	unsigned n;
{
  return((RVector) allocate(n, sizeof(double)));
}

CVector cvector(n)
	unsigned n;
{
  return((CVector) allocate(n, sizeof(Complex)));
}

free_vector(v) Vector v; { free_alloc(v); }

IMatrix imatrix(n, m)
	unsigned n, m;
{
  int i;
  IMatrix mat = (IMatrix) allocate(n, sizeof(IVector));
  for (i = 0; i < n; i++) mat[i] = (IVector) allocate(m, sizeof(int));
  return(mat);
}

RMatrix rmatrix(n, m)
	unsigned n, m;
{
  int i;
  RMatrix mat = (RMatrix) allocate(n, sizeof(RVector));
  for (i = 0; i < n; i++) mat[i] = (RVector) allocate(m, sizeof(double));
  return(mat);
}

CMatrix cmatrix(n, m)
	unsigned n, m;
{
  int i;
  CMatrix mat = (CMatrix) allocate(n, sizeof(CVector));
  for (i = 0; i < n; i++) mat[i] = (CVector) allocate(m, sizeof(Complex));
  return(mat);
}

free_matrix(mat, n)
	Matrix mat;
	int n;
{
  int i;
  
  if (mat != nil) for (i = 0; i < n; i++) free_alloc(mat[i]);
  free_alloc(mat);
}
