/* eigen.f -- translated by f2c (version of 19 December 1990  16:50:21).
   and modified. */

#include "xmath.h"

#define integer int
#define real float
#define doublereal double
#define Min(x,y) ((x) > (y) ? (y) : (x))
#define Max(x,y) ((x) > (y) ? (x) : (y))
#define Abs(x) ((x) >= 0 ? (x) : -(x))

static /* Subroutine */ int tred2(), tql2();
static doublereal pythag(), d_sign();

/* Table of constant values */

static doublereal c_b39 = 1.;

/* Subroutine */ int eigen(nm, n, a, w, z, fv1, ierr)
integer *nm, *n;
doublereal *a, *w, *z, *fv1;
integer *ierr;
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS */
/*     OF A REAL SYMMETRIC MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A. */

/*        A  CONTAINS THE REAL SYMMETRIC MATRIX. */

/*     ON OUTPUT */

/*        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER. */

/*        Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR TQLRAT */
/*           AND TQL2.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        FV1 IS A TEMPORARY STORAGE ARRAY. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --fv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --w;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L10:
    tred2(nm, n, &a[a_offset], &w[1], &fv1[1], &z[z_offset]);
    tql2(nm, n, &w[1], &fv1[1], &z[z_offset], ierr);
L50:
    return 0;
} /* eigen */

static /* Subroutine */ int tred2(nm, n, a, d, e, z)
integer *nm, *n;
doublereal *a, *d, *e, *z;
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    static doublereal f, g, h;
    static integer i, j, k, l;
    static doublereal scale, hh;
    static integer ii, jp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED2, */
/*     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX TO A */
/*     SYMMETRIC TRIDIAGONAL MATRIX USING AND ACCUMULATING */
/*     ORTHOGONAL SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE */
/*          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED. */

/*     ON OUTPUT */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO. */

/*        Z CONTAINS THE ORTHOGONAL TRANSFORMATION MATRIX */
/*          PRODUCED IN THE REDUCTION. */

/*        A AND Z MAY COINCIDE.  IF DISTINCT, A IS UNALTERED. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --e;
    --d;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {

	i__2 = *n;
	for (j = i; j <= i__2; ++j) {
/* L80: */
	    z[j + i * z_dim1] = a[j + i * a_dim1];
	}

	d[i] = a[*n + i * a_dim1];
/* L100: */
    }

    if (*n == 1) {
	goto L510;
    }
/*     .......... FOR I=N STEP -1 UNTIL 2 DO -- .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i = *n + 2 - ii;
	l = i - 1;
	h = 0.;
	scale = 0.;
	if (l < 2) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale += (d__1 = d[k], Abs(d__1));
	}

	if (scale != 0.) {
	    goto L140;
	}
L130:
	e[i] = d[l];

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    d[j] = z[l + j * z_dim1];
	    z[i + j * z_dim1] = 0.;
	    z[j + i * z_dim1] = 0.;
/* L135: */
	}

	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d[k] /= scale;
	    h += d[k] * d[k];
/* L150: */
	}

	f = d[l];
	d__1 = sqrt(h);
	g = -d_sign(&d__1, &f);
	e[i] = scale * g;
	h -= f * g;
	d[l] = f - g;
/*     .......... FORM A*U .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L170: */
	    e[j] = 0.;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    z[j + i * z_dim1] = f;
	    g = e[j] + z[j + j * z_dim1] * f;
	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g += z[k + j * z_dim1] * d[k];
		e[k] += z[k + j * z_dim1] * f;
/* L200: */
	    }

L220:
	    e[j] = g;
/* L240: */
	}
/*     .......... FORM P .......... */
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h;
	    f += e[j] * d[j];
/* L245: */
	}

	hh = f / (h + h);
/*     .......... FORM Q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= hh * d[j];
	}
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d[j];
	    g = e[j];

	    i__3 = l;
	    for (k = j; k <= i__3; ++k) {
/* L260: */
		z[k + j * z_dim1] = z[k + j * z_dim1] - f * e[k] - g * d[k];
	    }

	    d[j] = z[l + j * z_dim1];
	    z[i + j * z_dim1] = 0.;
/* L280: */
	}

L290:
	d[i] = h;
/* L300: */
    }
/*     .......... ACCUMULATION OF TRANSFORMATION MATRICES .......... */
    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
	l = i - 1;
	z[*n + l * z_dim1] = z[l + l * z_dim1];
	z[l + l * z_dim1] = 1.;
	h = d[i];
	if (h == 0.) {
	    goto L380;
	}

	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L330: */
	    d[k] = z[k + i * z_dim1] / h;
	}

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
/* L340: */
		g += z[k + i * z_dim1] * z[k + j * z_dim1];
	    }

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		z[k + j * z_dim1] -= g * d[k];
/* L360: */
	    }
	}

L380:
	i__3 = l;
	for (k = 1; k <= i__3; ++k) {
/* L400: */
	    z[k + i * z_dim1] = 0.;
	}

/* L500: */
    }

L510:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d[i] = z[*n + i * z_dim1];
	z[*n + i * z_dim1] = 0.;
/* L520: */
    }

    z[*n + *n * z_dim1] = 1.;
    e[1] = 0.;
    return 0;
} /* tred2 */

static /* Subroutine */ int tql2(nm, n, d, e, z, ierr)
integer *nm, *n;
doublereal *d, *e, *z;
integer *ierr;
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal c, f, g, h;
    static integer i, j, k, l, m;
    static doublereal p, r, s, c2, c3;
    static integer l1, l2;
    static doublereal s2;
    static integer ii;
    static doublereal dl1, el1;
    static integer mml;
    static doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL2, */
/*     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND */
/*     WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS */
/*     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD. */
/*     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO */
/*     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS */
/*     FULL MATRIX TO TRIDIAGONAL FORM. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX */
/*          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE */
/*          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS */
/*          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN */
/*          THE IDENTITY MATRIX. */

/*      ON OUTPUT */

/*        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT */
/*          UNORDERED FOR INDICES 1,2,...,IERR-1. */

/*        E HAS BEEN DESTROYED. */

/*        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC */
/*          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE, */
/*          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED */
/*          EIGENVALUES. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE J-TH EIGENVALUE HAS NOT BEEN */
/*                     DETERMINED AFTER 30 ITERATIONS. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z -= z_offset;
    --e;
    --d;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
/* L100: */
	e[i - 1] = e[i];
    }

    f = 0.;
    tst1 = 0.;
    e[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h = (d__1 = d[l], Abs(d__1)) + (d__2 = e[l], Abs(d__2));
	if (tst1 < h) {
	    tst1 = h;
	}
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    tst2 = tst1 + (d__1 = e[m], Abs(d__1));
	    if (tst2 == tst1) {
		goto L120;
	    }
/*     .......... E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L220;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	l2 = l1 + 1;
	g = d[l];
	p = (d[l1] - g) / (e[l] * 2.);
	r = pythag(&p, &c_b39);
	d[l] = e[l] / (p + d_sign(&r, &p));
	d[l1] = e[l] * (p + d_sign(&r, &p));
	dl1 = d[l1];
	h = g - d[l];
	if (l2 > *n) {
	    goto L145;
	}

	i__2 = *n;
	for (i = l2; i <= i__2; ++i) {
/* L140: */
	    d[i] -= h;
	}

L145:
	f += h;
/*     .......... QL TRANSFORMATION .......... */
	p = d[m];
	c = 1.;
	c2 = c;
	el1 = e[l1];
	s = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    c3 = c2;
	    c2 = c;
	    s2 = s;
	    i = m - ii;
	    g = c * e[i];
	    h = c * p;
	    r = pythag(&p, &e[i]);
	    e[i + 1] = s * r;
	    s = e[i] / r;
	    c = p / r;
	    p = c * d[i] - s * g;
	    d[i + 1] = h + s * (c * g + s * d[i]);
/*     .......... FORM VECTOR .......... */
	    i__3 = *n;
	    for (k = 1; k <= i__3; ++k) {
		h = z[k + (i + 1) * z_dim1];
		z[k + (i + 1) * z_dim1] = s * z[k + i * z_dim1] + c * h;
		z[k + i * z_dim1] = c * z[k + i * z_dim1] - s * h;
/* L180: */
	    }

/* L200: */
	}

	p = -s * s2 * c3 * el1 * e[l] / dl1;
	e[l] = s * p;
	d[l] = c * p;
	tst2 = tst1 + (d__1 = e[l], Abs(d__1));
	if (tst2 > tst1) {
	    goto L130;
	}
L220:
	d[l] += f;
/* L240: */
    }
/*     .......... ORDER EIGENVALUES AND EIGENVECTORS .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i = ii - 1;
	k = i;
	p = d[i];

	i__2 = *n;
	for (j = ii; j <= i__2; ++j) {
	    if (d[j] >= p) {
		goto L260;
	    }
	    k = j;
	    p = d[j];
L260:
	    ;
	}

	if (k == i) {
	    goto L300;
	}
	d[k] = d[i];
	d[i] = p;

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    p = z[j + i * z_dim1];
	    z[j + i * z_dim1] = z[j + k * z_dim1];
	    z[j + k * z_dim1] = p;
/* L280: */
	}

L300:
	;
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tql2 */

static doublereal pythag(a, b)
doublereal *a, *b;
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Local variables */
    static doublereal p, r, s, t, u;


/*     FINDS DSQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW */


/* Computing MAX */
    d__1 = Abs(*a), d__2 = Abs(*b);
    p = Max(d__1,d__2);
    if (p == 0.) {
	goto L20;
    }
/* Computing MIN */
    d__2 = Abs(*a), d__3 = Abs(*b);
/* Computing 2nd power */
    d__1 = Min(d__2,d__3) / p;
    r = d__1 * d__1;
L10:
    t = r + 4.;
    if (t == 4.) {
	goto L20;
    }
    s = r / t;
    u = s * 2. + 1.;
    p = u * p;
/* Computing 2nd power */
    d__1 = s / u;
    r = d__1 * d__1 * r;
    goto L10;
L20:
    ret_val = p;
    return ret_val;
} /* pythag */

static double d_sign(a,b)
     doublereal *a, *b;
{
  double x;
  x = (*a >= 0 ? *a : - *a);
  return( *b >= 0 ? x : -x);
}
