/* from fftpkg package in cmlib and netlib -- translated by f2c and modified */

#include "xmath.h"

/*
  Public Routine
*/

/*
 * cfft computes the forward or backward complex discrete fourier transform. 
 *
 * Input Parameters:
 *
 * n      The length of the complex sequence c. The method is
 *        more efficient when n is the product of small primes.
 *
 * c      A complex array of length n which contains the sequence
 *
 * wsave  a real work array which must be dimensioned at least 4n+15
 *        in the program that calls cfft.
 * isign  1 for transform, -1 for inverse transform.
 *        A call of cfft with isign = 1 followed by a call of cfft with 
 *        isign = -1 will multiply the sequence by n.
 *
 * Output Parameters:
 *
 * c      For j=1,...,n
 *
 *             c(j)=the sum from k=1,...,n of
 *
 *                   c(k)*exp(-isign*i*(j-1)*(k-1)*2*pi/n)
 *
 *                         where i=sqrt(-1)
 */

int cfft(n, c, wsave, isign)
     int n;
     double *c, *wsave;
     int isign;
{
  int iw1, iw2;
  
  /* Parameter adjustments */
  --c;
  --wsave;

  /* Function Body */
  if (n != 1) {
    iw1 = n + n + 1;
    iw2 = iw1 + n + n;
    cffti1_(&n, &wsave[iw1], &wsave[iw2]);
    cfft1_(&n, &c[1], &wsave[1], &wsave[iw1], &wsave[iw2], &isign);
  }
  return 0;
}

/*
  Internal Routines
*/

static int cfft1_(n, c, ch, wa, ifac, isign)
     int *n;
     double *c, *ch, *wa;
     int *ifac;
     int *isign;
{
  /* System generated locals */
  int i_1;

  /* Local variables */
  int idot;
  int i, k1, l1, l2, n2, na, nf, ip, iw, ix2, ix3, ix4, nac, ido, idl1;

  /* Parameter adjustments */
  --c;
  --ch;
  --wa;
  --ifac;
  
  /* Function Body */
  nf = ifac[2];
  na = 0;
  l1 = 1;
  iw = 1;
  i_1 = nf;
  for (k1 = 1; k1 <= i_1; ++k1) {
    ip = ifac[k1 + 2];
    l2 = ip * l1;
    ido = *n / l2;
    idot = ido + ido;
    idl1 = idot * l1;
    if (ip == 4) {
      ix2 = iw + idot;
      ix3 = ix2 + idot;
      if (na == 0) {
	pass4_(&idot, &l1, &c[1], &ch[1], &wa[iw], &wa[ix2], &wa[ix3], isign);
      }
      else {
	pass4_(&idot, &l1, &ch[1], &c[1], &wa[iw], &wa[ix2], &wa[ix3], isign);
      }
      na = 1 - na;
    }
    else if (ip == 2) {
      if (na == 0) {
	pass2_(&idot, &l1, &c[1], &ch[1], &wa[iw], isign);
      }
      else {
	pass2_(&idot, &l1, &ch[1], &c[1], &wa[iw], isign);
      }
      na = 1 - na;
    }
    else if (ip == 3) {
      ix2 = iw + idot;
      if (na == 0) {
	pass3_(&idot, &l1, &c[1], &ch[1], &wa[iw], &wa[ix2], isign);
      }
      else {
	pass3_(&idot, &l1, &ch[1], &c[1], &wa[iw], &wa[ix2], isign);
      }
      na = 1 - na;
    }
    else if (ip == 5) {
      ix2 = iw + idot;
      ix3 = ix2 + idot;
      ix4 = ix3 + idot;
      if (na == 0) {
	pass5_(&idot, &l1, &c[1], &ch[1], &wa[iw], &wa[ix2], &wa[ix3], 
	       &wa[ix4], isign);
      }
      else {
	pass5_(&idot, &l1, &ch[1], &c[1], &wa[iw], &wa[ix2], &wa[ix3], 
	       &wa[ix4], isign);
      }
      na = 1 - na;
    }
    else {
      if (na == 0) {
	pass_(&nac, &idot, &ip, &l1, &idl1, &c[1], &c[1], &c[1], &ch[1], 
	      &ch[1], &wa[iw], isign);
      }
      else {
	pass_(&nac, &idot, &ip, &l1, &idl1, &ch[1], &ch[1], &ch[1], &c[1], 
	      &c[1], &wa[iw], isign);
      }
      if (nac != 0) {
	na = 1 - na;
      }
    }
    l1 = l2;
    iw += (ip - 1) * idot;
  }
  if (na != 0) {
    n2 = *n + *n;
    i_1 = n2;
    for (i = 1; i <= i_1; ++i) {
      c[i] = ch[i];
    }
  }
  return 0;
}

static int cffti1_(n, wa, ifac)
     int *n;
     double *wa;
     int *ifac;
{
  /* Initialized data */
  static int ntryh[4] = { 3,4,2,5 };

  /* System generated locals */
  int i_1, i_2, i_3;

  /* Local variables */
  double argh;
  int idot, ntry, i, j;
  double argld;
  int i1, k1, l1, l2, ib;
  double fi;
  int ld, ii, nf, ip, nl, nq, nr;
  double arg;
  int ido, ipm;
  double tpi;

  /* Parameter adjustments */
  --wa;
  --ifac;

  /* Function Body */
  nl = *n;
  nf = 0;
  j = 0;

 L101:
  ++j;
  if (j - 4 <= 0) ntry = ntryh[j - 1];
  else ntry += 2;
 L104:
  nq = nl / ntry;
  nr = nl - ntry * nq;
  if (nr != 0) goto L101;
  ++nf;
  ifac[nf + 2] = ntry;
  nl = nq;
  if (ntry == 2 && nf != 1) {
    i_1 = nf;
    for (i = 2; i <= i_1; ++i) {
      ib = nf - i + 2;
      ifac[ib + 2] = ifac[ib + 1];
    }
    ifac[3] = 2;
  }
  if (nl != 1) goto L104;

  ifac[1] = *n;
  ifac[2] = nf;
  tpi = 6.28318530717959;
  argh = tpi / (double) (*n);
  i = 2;
  l1 = 1;
  i_1 = nf;
  for (k1 = 1; k1 <= i_1; ++k1) {
    ip = ifac[k1 + 2];
    ld = 0;
    l2 = l1 * ip;
    ido = *n / l2;
    idot = ido + ido + 2;
    ipm = ip - 1;
    i_2 = ipm;
    for (j = 1; j <= i_2; ++j) {
      i1 = i;
      wa[i - 1] = 1.0;
      wa[i] = 0.0;
      ld += l1;
      fi = 0.0;
      argld = (double) ld * argh;
      i_3 = idot;
      for (ii = 4; ii <= i_3; ii += 2) {
	i += 2;
	fi += 1.0;
	arg = fi * argld;
	wa[i - 1] = cos(arg);
	wa[i] = sin(arg);
      }
      if (ip > 5) {
	wa[i1 - 1] = wa[i - 1];
	wa[i1] = wa[i];
      }
    }
    l1 = l2;
  }
  return 0;
}

static int pass_(nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa, isign)
     int *nac;
     int *ido, *ip, *l1, *idl1;
     double *cc, *c1, *c2, *ch, *ch2, *wa;
     int *isign;
{
  /* System generated locals */
  int ch_dim1, ch_dim2, ch_offset, cc_dim1, cc_dim2, cc_offset, c1_dim1,
      c1_dim2, c1_offset, c2_dim1, c2_offset, ch2_dim1, ch2_offset, 
      i_1, i_2, i_3;

  /* Local variables */
  int idij, idlj, idot, ipph, i, j, k, l, jc, lc, ik, idj, idl, inc, idp;
  double wai, war;
  int ipp2;

  /* Parameter adjustments */
  cc_dim1 = *ido;
  cc_dim2 = *ip;
  cc_offset = cc_dim1 * (cc_dim2 + 1) + 1;
  cc -= cc_offset;
  c1_dim1 = *ido;
  c1_dim2 = *l1;
  c1_offset = c1_dim1 * (c1_dim2 + 1) + 1;
  c1 -= c1_offset;
  c2_dim1 = *idl1;
  c2_offset = c2_dim1 + 1;
  c2 -= c2_offset;
  ch_dim1 = *ido;
  ch_dim2 = *l1;
  ch_offset = ch_dim1 * (ch_dim2 + 1) + 1;
  ch -= ch_offset;
  ch2_dim1 = *idl1;
  ch2_offset = ch2_dim1 + 1;
  ch2 -= ch2_offset;
  --wa;
  
  /* Function Body */
  idot = *ido / 2;
  ipp2 = *ip + 2;
  ipph = (*ip + 1) / 2;
  idp = *ip * *ido;
  
  if (*ido >= *l1) {
    i_1 = ipph;
    for (j = 2; j <= i_1; ++j) {
      jc = ipp2 - j;
      i_2 = *l1;
      for (k = 1; k <= i_2; ++k) {
	i_3 = *ido;
	for (i = 1; i <= i_3; ++i) {
	  ch[i + (k + j * ch_dim2) * ch_dim1] =
	    cc[i + (j + k * cc_dim2) * cc_dim1] 
	      + cc[i + (jc + k * cc_dim2) * cc_dim1];
	  ch[i + (k + jc * ch_dim2) * ch_dim1] =
	    cc[i + (j + k * cc_dim2) * cc_dim1]
	      - cc[i + (jc + k * cc_dim2) * cc_dim1];
	}
      }
    }
    i_1 = *l1;
    for (k = 1; k <= i_1; ++k) {
      i_2 = *ido;
      for (i = 1; i <= i_2; ++i) {
	ch[i + (k + ch_dim2) * ch_dim1] = cc[i + (k * cc_dim2 + 1) * cc_dim1];
      }
    }
  }
  else {
    i_1 = ipph;
    for (j = 2; j <= i_1; ++j) {
      jc = ipp2 - j;
      i_2 = *ido;
      for (i = 1; i <= i_2; ++i) {
	i_3 = *l1;
	for (k = 1; k <= i_3; ++k) {
	  ch[i + (k + j * ch_dim2) * ch_dim1] = 
	    cc[i + (j + k * cc_dim2) * cc_dim1] 
	      + cc[i + (jc + k * cc_dim2) * cc_dim1];
	  ch[i + (k + jc * ch_dim2) * ch_dim1] =
	    cc[i + (j + k * cc_dim2) * cc_dim1] 
	      - cc[i + (jc + k * cc_dim2) * cc_dim1];
	}
      }
    }
    i_1 = *ido;
    for (i = 1; i <= i_1; ++i) {
      i_2 = *l1;
      for (k = 1; k <= i_2; ++k) {
	ch[i + (k + ch_dim2) * ch_dim1] = cc[i + (k * cc_dim2 + 1) * cc_dim1];
      }
    }
  }
  idl = 2 - *ido;
  inc = 0;
  i_1 = ipph;
  for (l = 2; l <= i_1; ++l) {
    lc = ipp2 - l;
    idl += *ido;
    i_2 = *idl1;
    for (ik = 1; ik <= i_2; ++ik) {
      c2[ik + l * c2_dim1] = ch2[ik + ch2_dim1] + wa[idl - 1] 
	* ch2[ik + (ch2_dim1 << 1)];
      c2[ik + lc * c2_dim1] = - *isign * wa[idl] * ch2[ik + *ip * ch2_dim1];
    }
    idlj = idl;
    inc += *ido;
    i_2 = ipph;
    for (j = 3; j <= i_2; ++j) {
      jc = ipp2 - j;
      idlj += inc;
      if (idlj > idp) {
	idlj -= idp;
      }
      war = wa[idlj - 1];
      wai = wa[idlj];
      i_3 = *idl1;
      for (ik = 1; ik <= i_3; ++ik) {
	c2[ik + l * c2_dim1] += war * ch2[ik + j * ch2_dim1];
	c2[ik + lc * c2_dim1] -= *isign * wai * ch2[ik + jc * ch2_dim1];
      }
    }
  }
  i_1 = ipph;
  for (j = 2; j <= i_1; ++j) {
    i_2 = *idl1;
    for (ik = 1; ik <= i_2; ++ik) {
      ch2[ik + ch2_dim1] += ch2[ik + j * ch2_dim1];
    }
  }
  i_1 = ipph;
  for (j = 2; j <= i_1; ++j) {
    jc = ipp2 - j;
    i_2 = *idl1;
    for (ik = 2; ik <= i_2; ik += 2) {
      ch2[ik - 1 + j * ch2_dim1] = c2[ik - 1 + j * c2_dim1]
	- c2[ik + jc * c2_dim1];
      ch2[ik - 1 + jc * ch2_dim1] = c2[ik - 1 + j * c2_dim1] 
	+ c2[ik + jc * c2_dim1];
      ch2[ik + j * ch2_dim1] = c2[ik + j * c2_dim1]
	+ c2[ik - 1 + jc * c2_dim1];
      ch2[ik + jc * ch2_dim1] = c2[ik + j * c2_dim1]
	- c2[ik - 1 + jc * c2_dim1];
    }
  }
  *nac = 1;
  if (*ido != 2) {
    *nac = 0;
    i_1 = *idl1;
    for (ik = 1; ik <= i_1; ++ik) {
      c2[ik + c2_dim1] = ch2[ik + ch2_dim1];
    }
    i_1 = *ip;
    for (j = 2; j <= i_1; ++j) {
      i_2 = *l1;
      for (k = 1; k <= i_2; ++k) {
	c1[(k + j * c1_dim2) * c1_dim1 + 1] = 
	  ch[(k + j * ch_dim2) * ch_dim1 + 1];
	c1[(k + j * c1_dim2) * c1_dim1 + 2] =
	  ch[(k + j * ch_dim2) * ch_dim1 + 2];
      }
    }
    if (idot <= *l1) {
      idij = 0;
      i_1 = *ip;
      for (j = 2; j <= i_1; ++j) {
	idij += 2;
	i_2 = *ido;
	for (i = 4; i <= i_2; i += 2) {
	  idij += 2;
	  i_3 = *l1;
	  for (k = 1; k <= i_3; ++k) {
	    c1[i - 1 + (k + j * c1_dim2) * c1_dim1] = wa[idij - 1] 
	      * ch[i - 1 + (k + j * ch_dim2) * ch_dim1] + *isign * wa[idij]
		* ch[i + (k + j * ch_dim2) * ch_dim1];
	    c1[i + (k + j * c1_dim2) * c1_dim1] = wa[idij - 1] 
	      * ch[i + (k + j * ch_dim2) * ch_dim1] - *isign * wa[idij] 
		* ch[i - 1 + (k + j * ch_dim2) * ch_dim1];
	  }
	}
      }
    } 
    else {
      idj = 2 - *ido;
      i_1 = *ip;
      for (j = 2; j <= i_1; ++j) {
	idj += *ido;
	i_2 = *l1;
	for (k = 1; k <= i_2; ++k) {
	  idij = idj;
	  i_3 = *ido;
	  for (i = 4; i <= i_3; i += 2) {
	    idij += 2;
	    c1[i - 1 + (k + j * c1_dim2) * c1_dim1] = wa[idij - 1] 
	      * ch[i - 1 + (k + j * ch_dim2) * ch_dim1] 
		+ *isign * wa[idij] * ch[i + (k + j * ch_dim2) * ch_dim1];
	    c1[i + (k + j * c1_dim2) * c1_dim1] = wa[idij - 1] 
	      * ch[i + (k + j * ch_dim2) * ch_dim1] - *isign * wa[idij] 
		* ch[i - 1 + (k + j * ch_dim2) * ch_dim1];
	  }
	}
      }
    }
  }
  return 0;
}

static int pass2_(ido, l1, cc, ch, wa1, isign)
     int *ido, *l1;
     double *cc, *ch, *wa1;
     int *isign;
{
  /* System generated locals */
  int cc_dim1, cc_offset, ch_dim1, ch_dim2, ch_offset, i_1, i_2;
  
  /* Local variables */
  int i, k;
  double ti2, tr2;
  
  /* Parameter adjustments */
  cc_dim1 = *ido;
  cc_offset = cc_dim1 * 3 + 1;
  cc -= cc_offset;
  ch_dim1 = *ido;
  ch_dim2 = *l1;
  ch_offset = ch_dim1 * (ch_dim2 + 1) + 1;
  ch -= ch_offset;
  --wa1;
  
  /* Function Body */
  if (*ido <= 2) {
    i_1 = *l1;
    for (k = 1; k <= i_1; ++k) {
      ch[(k + ch_dim2) * ch_dim1 + 1] = cc[((k << 1) + 1) * cc_dim1 + 1]
	+ cc[((k << 1) + 2) * cc_dim1 + 1];
      ch[(k + (ch_dim2 << 1)) * ch_dim1 + 1] = cc[((k << 1) + 1) * cc_dim1 + 1]
	- cc[((k << 1) + 2) * cc_dim1 + 1];
      ch[(k + ch_dim2) * ch_dim1 + 2] = cc[((k << 1) + 1) * cc_dim1 + 2]
	+ cc[((k << 1) + 2) * cc_dim1 + 2];
      ch[(k + (ch_dim2 << 1)) * ch_dim1 + 2] = cc[((k << 1) + 1) * cc_dim1 + 2]
	- cc[((k << 1) + 2) * cc_dim1 + 2];
    }
  }
  else {
    i_1 = *l1;
    for (k = 1; k <= i_1; ++k) {
      i_2 = *ido;
      for (i = 2; i <= i_2; i += 2) {
	ch[i - 1 + (k + ch_dim2) * ch_dim1] 
	  = cc[i - 1 + ((k << 1) + 1) * cc_dim1] 
	    + cc[i - 1 + ((k << 1) + 2) * cc_dim1];
	tr2 = cc[i - 1 + ((k << 1) + 1) * cc_dim1] 
	  - cc[i - 1 + ((k << 1) + 2) * cc_dim1];
	ch[i + (k + ch_dim2) * ch_dim1] = cc[i + ((k << 1) + 1) * cc_dim1] 
	  + cc[i + ((k << 1) + 2) * cc_dim1];
	ti2 = cc[i + ((k << 1) + 1) * cc_dim1] 
	  - cc[i + ((k << 1) + 2) * cc_dim1];
	ch[i + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i - 1] * ti2 
	  - *isign * wa1[i] * tr2;
	ch[i - 1 + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i - 1] * tr2 
	  + *isign * wa1[i] * ti2;
      }
    }
  }
  return 0;
}

static int pass3_(ido, l1, cc, ch, wa1, wa2, isign)
     int *ido, *l1;
     double *cc, *ch, *wa1, *wa2;
     int *isign;
{
  /* System generated locals */
  int cc_dim1, cc_offset, ch_dim1, ch_dim2, ch_offset, i_1, i_2;

  /* Local variables */
  double taui, taur;
  int i, k;
  double ci2, ci3, di2, di3, cr2, cr3, dr2, dr3, ti2, tr2;

  /* Parameter adjustments */
  cc_dim1 = *ido;
  cc_offset = (cc_dim1 << 2) + 1;
  cc -= cc_offset;
  ch_dim1 = *ido;
  ch_dim2 = *l1;
  ch_offset = ch_dim1 * (ch_dim2 + 1) + 1;
  ch -= ch_offset;
  --wa1;
  --wa2;

  /* Function Body */
  taur = -.5;
  taui = -(*isign) * .866025403784439;
  if (*ido == 2) {
    i_1 = *l1;
    for (k = 1; k <= i_1; ++k) {
      tr2 = cc[(k * 3 + 2) * cc_dim1 + 1] + cc[(k * 3 + 3) * cc_dim1 + 1];
      cr2 = cc[(k * 3 + 1) * cc_dim1 + 1] + taur * tr2;
      ch[(k + ch_dim2) * ch_dim1 + 1] = cc[(k * 3 + 1) * cc_dim1 + 1] + tr2;
      ti2 = cc[(k * 3 + 2) * cc_dim1 + 2] + cc[(k * 3 + 3) * cc_dim1 + 2];
      ci2 = cc[(k * 3 + 1) * cc_dim1 + 2] + taur * ti2;
      ch[(k + ch_dim2) * ch_dim1 + 2] = cc[(k * 3 + 1) * cc_dim1 + 2] + ti2;
      cr3 = taui * (cc[(k * 3 + 2) * cc_dim1 + 1] 
		    - cc[(k * 3 + 3) * cc_dim1 + 1]);
      ci3 = taui * (cc[(k * 3 + 2) * cc_dim1 + 2] 
		    - cc[(k * 3 + 3) * cc_dim1 + 2]);
      ch[(k + (ch_dim2 << 1)) * ch_dim1 + 1] = cr2 - ci3;
      ch[(k + ch_dim2 * 3) * ch_dim1 + 1] = cr2 + ci3;
      ch[(k + (ch_dim2 << 1)) * ch_dim1 + 2] = ci2 + cr3;
      ch[(k + ch_dim2 * 3) * ch_dim1 + 2] = ci2 - cr3;
    }
  }
  else {
    i_1 = *l1;
    for (k = 1; k <= i_1; ++k) {
      i_2 = *ido;
      for (i = 2; i <= i_2; i += 2) {
	tr2 = cc[i - 1 + (k * 3 + 2) * cc_dim1] 
	  + cc[i - 1 + (k * 3 + 3) * cc_dim1];
	cr2 = cc[i - 1 + (k * 3 + 1) * cc_dim1] + taur * tr2;
	ch[i - 1 + (k + ch_dim2) * ch_dim1] = cc[i - 1 + (k * 3 + 1)
						 * cc_dim1] + tr2;
	ti2 = cc[i + (k * 3 + 2) * cc_dim1] + cc[i + (k * 3 + 3) * cc_dim1];
	ci2 = cc[i + (k * 3 + 1) * cc_dim1] + taur * ti2;
	ch[i + (k + ch_dim2) * ch_dim1] = cc[i + (k * 3 + 1) * cc_dim1] + ti2;
	cr3 = taui * (cc[i - 1 + (k * 3 + 2) * cc_dim1] 
		      - cc[i - 1 + (k * 3 + 3) * cc_dim1]);
	ci3 = taui * (cc[i + (k * 3 + 2) * cc_dim1] 
		      - cc[i + (k * 3 + 3) * cc_dim1]);
	dr2 = cr2 - ci3;
	dr3 = cr2 + ci3;
	di2 = ci2 + cr3;
	di3 = ci2 - cr3;
	ch[i + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i - 1] * di2 
	  - *isign * wa1[i] * dr2;
	ch[i - 1 + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i - 1] * dr2 
	  + *isign * wa1[i] * di2;
	ch[i + (k + ch_dim2 * 3) * ch_dim1] = wa2[i - 1] * di3 
	  - *isign * wa2[i] * dr3;
	ch[i - 1 + (k + ch_dim2 * 3) * ch_dim1] = wa2[i - 1] * dr3 
	  + *isign * wa2[i] * di3;
      }
    }
  }
  return 0;
}

static int pass4_(ido, l1, cc, ch, wa1, wa2, wa3, isign)
     int *ido, *l1;
     double *cc, *ch, *wa1, *wa2, *wa3;
     int *isign;
{
  /* System generated locals */
  int cc_dim1, cc_offset, ch_dim1, ch_dim2, ch_offset, i_1, i_2;

  /* Local variables */
  int i, k;
  double ci2, ci3, ci4, cr2, cr3, cr4, ti1, ti2, ti3, ti4, tr1, tr2, tr3, tr4;

  /* Parameter adjustments */
  cc_dim1 = *ido;
  cc_offset = cc_dim1 * 5 + 1;
  cc -= cc_offset;
  ch_dim1 = *ido;
  ch_dim2 = *l1;
  ch_offset = ch_dim1 * (ch_dim2 + 1) + 1;
  ch -= ch_offset;
  --wa1;
  --wa2;
  --wa3;
  
  /* Function Body */
  if (*ido == 2) {
    i_1 = *l1;
    for (k = 1; k <= i_1; ++k) {
      ti1 = cc[((k << 2) + 1) * cc_dim1 + 2] 
	- cc[((k << 2) + 3) * cc_dim1 + 2];
      ti2 = cc[((k << 2) + 1) * cc_dim1 + 2] 
	+ cc[((k << 2) + 3) * cc_dim1 + 2];
      tr4 = *isign * (cc[((k << 2) + 2) * cc_dim1 + 2] 
		      - cc[((k << 2) + 4) * cc_dim1 + 2]);
      ti3 = cc[((k << 2) + 2) * cc_dim1 + 2] 
	+ cc[((k << 2) + 4) * cc_dim1 + 2];
      tr1 = cc[((k << 2) + 1) * cc_dim1 + 1] 
	- cc[((k << 2) + 3) * cc_dim1 + 1];
      tr2 = cc[((k << 2) + 1) * cc_dim1 + 1] 
	+ cc[((k << 2) + 3) * cc_dim1 + 1];
      ti4 = *isign * (cc[((k << 2) + 4) * cc_dim1 + 1] 
		      - cc[((k << 2) + 2) * cc_dim1 + 1]);
      tr3 = cc[((k << 2) + 2) * cc_dim1 + 1] 
	+ cc[((k << 2) + 4) * cc_dim1 + 1];
      ch[(k + ch_dim2) * ch_dim1 + 1] = tr2 + tr3;
      ch[(k + ch_dim2 * 3) * ch_dim1 + 1] = tr2 - tr3;
      ch[(k + ch_dim2) * ch_dim1 + 2] = ti2 + ti3;
      ch[(k + ch_dim2 * 3) * ch_dim1 + 2] = ti2 - ti3;
      ch[(k + (ch_dim2 << 1)) * ch_dim1 + 1] = tr1 + tr4;
      ch[(k + (ch_dim2 << 2)) * ch_dim1 + 1] = tr1 - tr4;
      ch[(k + (ch_dim2 << 1)) * ch_dim1 + 2] = ti1 + ti4;
      ch[(k + (ch_dim2 << 2)) * ch_dim1 + 2] = ti1 - ti4;
    }
  }
  else {
    i_1 = *l1;
    for (k = 1; k <= i_1; ++k) {
      i_2 = *ido;
      for (i = 2; i <= i_2; i += 2) {
	ti1 = cc[i + ((k << 2) + 1) * cc_dim1] 
	  - cc[i + ((k << 2) + 3) * cc_dim1];
	ti2 = cc[i + ((k << 2) + 1) * cc_dim1] 
	  + cc[i + ((k << 2) + 3) * cc_dim1];
	ti3 = cc[i + ((k << 2) + 2) * cc_dim1] 
	  + cc[i + ((k << 2) + 4) * cc_dim1];
	tr4 = *isign * (cc[i + ((k << 2) + 2) * cc_dim1] 
			- cc[i + ((k << 2) + 4) * cc_dim1]);
	tr1 = cc[i - 1 + ((k << 2) + 1) * cc_dim1] 
	  - cc[i - 1 + ((k << 2) + 3) * cc_dim1];
	tr2 = cc[i - 1 + ((k << 2) + 1) * cc_dim1] 
	  + cc[i - 1 + ((k << 2) + 3) * cc_dim1];
	ti4 = *isign * (cc[i - 1 + ((k << 2) + 4) * cc_dim1]
			- cc[i - 1 + ((k << 2) + 2) * cc_dim1]);
	tr3 = cc[i - 1 + ((k << 2) + 2) * cc_dim1] 
	  + cc[i - 1 + ((k << 2) + 4) * cc_dim1];
	ch[i - 1 + (k + ch_dim2) * ch_dim1] = tr2 + tr3;
	cr3 = tr2 - tr3;
	ch[i + (k + ch_dim2) * ch_dim1] = ti2 + ti3;
	ci3 = ti2 - ti3;
	cr2 = tr1 + tr4;
	cr4 = tr1 - tr4;
	ci2 = ti1 + ti4;
	ci4 = ti1 - ti4;
	ch[i - 1 + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i - 1] * cr2 
	  + *isign * wa1[i] * ci2;
	ch[i + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i - 1] * ci2 
	  - *isign * wa1[i] * cr2;
	ch[i - 1 + (k + ch_dim2 * 3) * ch_dim1] = wa2[i - 1] * cr3
	  + *isign * wa2[i] * ci3;
	ch[i + (k + ch_dim2 * 3) * ch_dim1] = wa2[i - 1] * ci3 
	  - *isign * wa2[i] * cr3;
	ch[i - 1 + (k + (ch_dim2 << 2)) * ch_dim1] = wa3[i - 1] * cr4 
	  + *isign * wa3[i] * ci4;
	ch[i + (k + (ch_dim2 << 2)) * ch_dim1] = wa3[i - 1] * ci4 
	  - *isign * wa3[i] * cr4;
      }
    }
  }
  return 0;
}

static int pass5_(ido, l1, cc, ch, wa1, wa2, wa3, wa4, isign)
     int *ido, *l1;
     double *cc, *ch, *wa1, *wa2, *wa3, *wa4;
     int *isign;
{
  /* System generated locals */
  int cc_dim1, cc_offset, ch_dim1, ch_dim2, ch_offset, i_1, i_2;

  /* Local variables */
  int i, k;
  double ci2, ci3, ci4, ci5, di3, di4, di5, di2, cr2, cr3, cr5, cr4, ti2, ti3,
         ti4, ti5, dr3, dr4, dr5, dr2, tr2, tr3, tr4, tr5, ti11, ti12, tr11,
         tr12;

  /* Parameter adjustments */
  cc_dim1 = *ido;
  cc_offset = cc_dim1 * 6 + 1;
  cc -= cc_offset;
  ch_dim1 = *ido;
  ch_dim2 = *l1;
  ch_offset = ch_dim1 * (ch_dim2 + 1) + 1;
  ch -= ch_offset;
  --wa1;
  --wa2;
  --wa3;
  --wa4;
  
  /* Function Body */
  tr11 = .309016994374947;
  ti11 = -(*isign) * .951056516295154;
  tr12 = -.809016994374947;
  ti12 = -(*isign) * .587785252292473;
  if (*ido == 2) {
    i_1 = *l1;
    for (k = 1; k <= i_1; ++k) {
      ti5 = cc[(k * 5 + 2) * cc_dim1 + 2] - cc[(k * 5 + 5) * cc_dim1 + 2];
      ti2 = cc[(k * 5 + 2) * cc_dim1 + 2] + cc[(k * 5 + 5) * cc_dim1 + 2];
      ti4 = cc[(k * 5 + 3) * cc_dim1 + 2] - cc[(k * 5 + 4) * cc_dim1 + 2];
      ti3 = cc[(k * 5 + 3) * cc_dim1 + 2] + cc[(k * 5 + 4) * cc_dim1 + 2];
      tr5 = cc[(k * 5 + 2) * cc_dim1 + 1] - cc[(k * 5 + 5) * cc_dim1 + 1];
      tr2 = cc[(k * 5 + 2) * cc_dim1 + 1] + cc[(k * 5 + 5) * cc_dim1 + 1];
      tr4 = cc[(k * 5 + 3) * cc_dim1 + 1] - cc[(k * 5 + 4) * cc_dim1 + 1];
      tr3 = cc[(k * 5 + 3) * cc_dim1 + 1] + cc[(k * 5 + 4) * cc_dim1 + 1];
      ch[(k + ch_dim2) * ch_dim1 + 1] = cc[(k * 5 + 1) * cc_dim1 + 1] 
	+ tr2 + tr3;
      ch[(k + ch_dim2) * ch_dim1 + 2] = cc[(k * 5 + 1) * cc_dim1 + 2] 
	+ ti2 + ti3;
      cr2 = cc[(k * 5 + 1) * cc_dim1 + 1] + tr11 * tr2 + tr12 * tr3;
      ci2 = cc[(k * 5 + 1) * cc_dim1 + 2] + tr11 * ti2 + tr12 * ti3;
      cr3 = cc[(k * 5 + 1) * cc_dim1 + 1] + tr12 * tr2 + tr11 * tr3;
      ci3 = cc[(k * 5 + 1) * cc_dim1 + 2] + tr12 * ti2 + tr11 * ti3;
      cr5 = ti11 * tr5 + ti12 * tr4;
      ci5 = ti11 * ti5 + ti12 * ti4;
      cr4 = ti12 * tr5 - ti11 * tr4;
      ci4 = ti12 * ti5 - ti11 * ti4;
      ch[(k + (ch_dim2 << 1)) * ch_dim1 + 1] = cr2 - ci5;
      ch[(k + ch_dim2 * 5) * ch_dim1 + 1] = cr2 + ci5;
      ch[(k + (ch_dim2 << 1)) * ch_dim1 + 2] = ci2 + cr5;
      ch[(k + ch_dim2 * 3) * ch_dim1 + 2] = ci3 + cr4;
      ch[(k + ch_dim2 * 3) * ch_dim1 + 1] = cr3 - ci4;
      ch[(k + (ch_dim2 << 2)) * ch_dim1 + 1] = cr3 + ci4;
      ch[(k + (ch_dim2 << 2)) * ch_dim1 + 2] = ci3 - cr4;
      ch[(k + ch_dim2 * 5) * ch_dim1 + 2] = ci2 - cr5;
    }
  }
  else {
    i_1 = *l1;
    for (k = 1; k <= i_1; ++k) {
      i_2 = *ido;
      for (i = 2; i <= i_2; i += 2) {
	ti5 = cc[i + (k * 5 + 2) * cc_dim1] - cc[i + (k * 5 + 5) * cc_dim1];
	ti2 = cc[i + (k * 5 + 2) * cc_dim1] + cc[i + (k * 5 + 5) * cc_dim1];
	ti4 = cc[i + (k * 5 + 3) * cc_dim1] - cc[i + (k * 5 + 4) * cc_dim1];
	ti3 = cc[i + (k * 5 + 3) * cc_dim1] + cc[i + (k * 5 + 4) * cc_dim1];
	tr5 = cc[i - 1 + (k * 5 + 2) * cc_dim1] 
	  - cc[i - 1 + (k * 5 + 5) * cc_dim1];
	tr2 = cc[i - 1 + (k * 5 + 2) * cc_dim1] 
	  + cc[i - 1 + (k * 5 + 5) * cc_dim1];
	tr4 = cc[i - 1 + (k * 5 + 3) * cc_dim1] 
	  - cc[i - 1 + (k * 5 + 4) * cc_dim1];
	tr3 = cc[i - 1 + (k * 5 + 3) * cc_dim1] 
	  + cc[i - 1 + (k * 5 + 4) * cc_dim1];
	ch[i - 1 + (k + ch_dim2) * ch_dim1] = cc[i - 1 + (k * 5 + 1) 
						 * cc_dim1] + tr2 + tr3;
	ch[i + (k + ch_dim2) * ch_dim1] = cc[i + (k * 5 + 1) 
					     * cc_dim1] + ti2 + ti3;
	cr2 = cc[i - 1 + (k * 5 + 1) * cc_dim1] + tr11 * tr2 + tr12 * tr3;
	ci2 = cc[i + (k * 5 + 1) * cc_dim1] + tr11 * ti2 + tr12 * ti3;

	cr3 = cc[i - 1 + (k * 5 + 1) * cc_dim1] + tr12 * tr2 + tr11 * tr3;
	ci3 = cc[i + (k * 5 + 1) * cc_dim1] + tr12 * ti2 + tr11 * ti3;

	cr5 = ti11 * tr5 + ti12 * tr4;
	ci5 = ti11 * ti5 + ti12 * ti4;
	cr4 = ti12 * tr5 - ti11 * tr4;
	ci4 = ti12 * ti5 - ti11 * ti4;
	dr3 = cr3 - ci4;
	dr4 = cr3 + ci4;
	di3 = ci3 + cr4;
	di4 = ci3 - cr4;
	dr5 = cr2 + ci5;
	dr2 = cr2 - ci5;
	di5 = ci2 - cr5;
	di2 = ci2 + cr5;
	ch[i - 1 + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i - 1] * dr2 
	  + *isign * wa1[i] * di2;
	ch[i + (k + (ch_dim2 << 1)) * ch_dim1] = wa1[i - 1] * di2
	  - *isign * wa1[i] * dr2;
	ch[i - 1 + (k + ch_dim2 * 3) * ch_dim1] = wa2[i - 1] * dr3 
	  + *isign * wa2[i] * di3;
	ch[i + (k + ch_dim2 * 3) * ch_dim1] = wa2[i - 1] * di3 
	  - *isign * wa2[i] * dr3;
	ch[i - 1 + (k + (ch_dim2 << 2)) * ch_dim1] = wa3[i - 1] * dr4 
	  + *isign * wa3[i] * di4;
	ch[i + (k + (ch_dim2 << 2)) * ch_dim1] = wa3[i - 1] * di4
	  - *isign * wa3[i] * dr4;
	ch[i - 1 + (k + ch_dim2 * 5) * ch_dim1] = wa4[i - 1] * dr5 
	  + *isign * wa4[i] * di5;
	ch[i + (k + ch_dim2 * 5) * ch_dim1] = wa4[i - 1] * di5 
	  - *isign * wa4[i] * dr5;
      }
    }
  }
  return 0;
}
