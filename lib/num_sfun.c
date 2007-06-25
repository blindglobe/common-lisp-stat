/*
(c) Copyright Taiichi Yuasa and Masami Hagiya, 1984.  All rights reserved.
Copying of this file is authorized to users who have executed the true and
proper "License Agreement for Kyoto Common LISP" with SIGLISP.
*/

#include "include.h"
#include "num_include.h"

object imag_unit, minus_imag_unit, imag_two;

int
fixnum_expt(x, y)
{
	int z;

	z = 1;
	while (y > 0)
		if (y%2 == 0) {
			x *= x;
			y /= 2;
		} else {
			z *= x;
			--y;
		}
	return(z);
}

object
number_exp(x)
object x;
{
	double exp();

	switch (type_of(x)) {

	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_longfloat(exp(number_to_double(x))));

	case t_shortfloat:
		return(make_shortfloat((shortfloat)exp((double)(sf(x)))));

	case t_longfloat:
		return(make_longfloat(exp(lf(x))));

	case t_complex:
	{
		object y, y1;
		object number_sin(), number_cos();
	        vs_mark;
	
		y = x->cmp.cmp_imag;
		x = x->cmp.cmp_real;
		x = number_exp(x);
		vs_push(x);
		y1 = number_cos(y);
		vs_push(y1);
		y = number_sin(y);
		vs_push(y);
		y = make_complex(y1, y);
		vs_push(y);
		x = number_times(x, y);
		vs_reset;
		return(x);
	}

	default:
		FEwrong_type_argument(Snumber, x);
	}
}

object
number_expt(x, y)
object x, y;
{
	enum type tx, ty;
	object z, number_nlog();
	vs_mark;

	tx = type_of(x);
	ty = type_of(y);
	if (ty == t_fixnum && fix(y) == 0)
		switch (tx) {
		case t_fixnum:  case t_bignum:  case t_ratio:
			return(small_fixnum(1));

		case t_shortfloat:
			return(make_shortfloat((shortfloat)1.0));

		case t_longfloat:
			return(make_longfloat(1.0));

		case t_complex:
			z = number_expt(x->cmp.cmp_real, y);
			vs_push(z);
			z = make_complex(z, small_fixnum(0));
			vs_reset;
			return(z);

		default:
			FEwrong_type_argument(Snumber, x);
		}
	if (number_zerop(x)) {
		if (!number_plusp(ty==t_complex?y->cmp.cmp_real:y))
			FEerror("Cannot raise zero to the power ~S.", 1, y);
		return(number_times(x, y));
	}
	if (ty == t_fixnum || ty == t_bignum) {
		if (number_minusp(y)) {
			z = number_negate(y);
			vs_push(z);
			z = number_expt(x, z);
			vs_push(z);
			z = number_divide(small_fixnum(1), z);
			vs_reset;
			return(z);
		}
		z = small_fixnum(1);
		vs_push(z);
		vs_push(Cnil);
		vs_push(Cnil);
		while (number_plusp(y))
			if (number_evenp(y)) {
				x = number_times(x, x);
				vs_top[-1] = x;
				y = integer_divide1(y, small_fixnum(2));
				vs_top[-2] = y;
			} else {
				z = number_times(z, x);
				vs_top[-3] = z;
				y = number_minus(y, small_fixnum(1));
				vs_top[-2] = y;
			}
		vs_reset;
		return(z);
	}
	z = number_nlog(x);
	vs_push(z);
	z = number_times(z, y);
	vs_push(z);
	z = number_exp(z);
	vs_reset;
	return(z);
}

object
number_nlog(x)
object x;
{
	double log();
	object r, i, a, p, number_sqrt(), number_atan2();
	vs_mark;

	if (type_of(x) == t_complex) {
		r = x->cmp.cmp_real;
		i = x->cmp.cmp_imag;
		goto COMPLEX;
	}
	if (number_zerop(x))
		FEerror("Zero is the logarithmic singularity.", 0);
	if (number_minusp(x)) {
		r = x;
		i = small_fixnum(0);
		goto COMPLEX;
	}
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_longfloat(log(number_to_double(x))));

	case t_shortfloat:
		return(make_shortfloat((shortfloat)log((double)(sf(x)))));

	case t_longfloat:
		return(make_longfloat(log(lf(x))));

	default:
		FEwrong_type_argument(Snumber, x);
	}

COMPLEX:
	a = number_times(r, r);
	vs_push(a);
	p = number_times(i, i);
	vs_push(p);
	a = number_plus(a, p);
	vs_push(a);
	a = number_nlog(a);
	vs_push(a);
	a = number_divide(a, small_fixnum(2));
	vs_push(a);
	p = number_atan2(i, r);
	vs_push(p);
	x = make_complex(a, p);
	vs_reset;
	return(x);
}

object
number_log(x, y)
object x, y;
{
	object z;
	vs_mark;

	if (number_zerop(y))
		FEerror("Zero is the logarithmic singularity.", 0);
	if (number_zerop(x))
		return(number_times(x, y));
	x = number_nlog(x);
	vs_push(x);
	y = number_nlog(y);
	vs_push(y);
	z = number_divide(y, x);
	vs_reset;
	return(z);
}

object
number_sqrt(x)
object x;
{
	object z;
	double sqrt();
	vs_mark;

	if (type_of(x) == t_complex)
		goto COMPLEX;
	if (number_minusp(x))
		goto COMPLEX;
	switch (type_of(x)) {
	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_longfloat(sqrt(number_to_double(x))));

	case t_shortfloat:
		return(make_shortfloat((shortfloat)sqrt((double)(sf(x)))));

	case t_longfloat:
		return(make_longfloat(sqrt(lf(x))));

	default:
		FEwrong_type_argument(Snumber, x);
	}

COMPLEX:
	z = make_ratio(small_fixnum(1), small_fixnum(2));
	vs_push(z);
	z = number_expt(x, z);
	vs_reset;
	return(z);
}

object
number_atan2(y, x)
object y, x;
{
	object z;
	double atan(), dy, dx, dz;

	dy = number_to_double(y);
	dx = number_to_double(x);
	if (dx > 0.0)
		if (dy > 0.0)
			dz = atan(dy / dx);
		else if (dy == 0.0)
			dz = 0.0;
		else
			dz = -atan(-dy / dx);
	else if (dx == 0.0)
		if (dy > 0.0)
			dz = PI / 2.0;
		else if (dy == 0.0)
			FEerror("Logarithmic singularity.", 0);
		else
			dz = -PI / 2.0;
	else
		if (dy > 0.0)
			dz = PI - atan(dy / -dx);
		else if (dy == 0.0)
			dz = PI;
		else
			dz = -PI + atan(-dy / -dx);
	z = make_longfloat(dz);
	return(z);
}

object
number_atan(y)
object y;
{
	object z, z1;
        vs_mark;

	if (type_of(y) == t_complex) {
		z = number_times(imag_unit, y);
		vs_push(z);
		z = one_plus(z);
		vs_push(z);
		z1 = number_times(y, y);
		vs_push(z1);
		z1 = one_plus(z1);
		vs_push(z1);
		z1 = number_sqrt(z1);
		vs_push(z1);
		z = number_divide(z, z1);
		vs_push(z);
		z = number_nlog(z);
		vs_push(z);
		z = number_times(minus_imag_unit, z);
		vs_reset;
		return(z);
	}
	return(number_atan2(y, small_fixnum(1)));
}

object
number_sin(x)
object x;
{
	double sin();

	switch (type_of(x)) {

	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_longfloat(sin(number_to_double(x))));

	case t_shortfloat:
		return(make_shortfloat((shortfloat)sin((double)(sf(x)))));

	case t_longfloat:
		return(make_longfloat(sin(lf(x))));

	case t_complex:
	{
		object	r;
		object	x0, x1, x2;
		vs_mark;

		x0 = number_times(imag_unit, x);
		vs_push(x0);
		x0 = number_exp(x0);
		vs_push(x0);
		x1 = number_times(minus_imag_unit, x);
		vs_push(x1);
		x1 = number_exp(x1);
		vs_push(x1);
		x2 = number_minus(x0, x1);
		vs_push(x2);
		r = number_divide(x2, imag_two);

		vs_reset;
		return(r);
	}

	default:
		FEwrong_type_argument(Snumber, x);

	}
}

object
number_cos(x)
object x;
{
	double cos();

	switch (type_of(x)) {

	case t_fixnum:
	case t_bignum:
	case t_ratio:
		return(make_longfloat(cos(number_to_double(x))));

	case t_shortfloat:
		return(make_shortfloat((shortfloat)cos((double)(sf(x)))));

	case t_longfloat:
		return(make_longfloat(cos(lf(x))));

	case t_complex:
	{
		object r;
		object x0, x1, x2;
		vs_mark;

		x0 = number_times(imag_unit, x);
		vs_push(x0);
		x0 = number_exp(x0);
		vs_push(x0);
		x1 = number_times(minus_imag_unit, x);
		vs_push(x1);
		x1 = number_exp(x1);
		vs_push(x1);
		x2 = number_plus(x0, x1);
		vs_push(x2);
		r = number_divide(x2, small_fixnum(2));

		vs_reset;
		return(r);
	}

	default:
		FEwrong_type_argument(Snumber, x);

	}
}

object
number_tan(x)
object x;
{
	object r, s, c;
	vs_mark;

	s = number_sin(x);
	vs_push(s);
	c = number_cos(x);
	vs_push(c);
	if (number_zerop(c) == TRUE)
		FEerror("Cannot compute the tangent of ~S.", 1, x);
	r = number_divide(s, c);
	vs_reset;
	return(r);
}

object
number_asin(x)
     object x;
{
  double asin();
  double dx;

  /* check for a real argument in [-1,1] */
  if (type_of(x) != t_complex) {
    switch (type_of(x)) {
    case t_fixnum:
    case t_bignum:
    case t_ratio:
      dx = number_to_double(x);
      break;
    case t_shortfloat:
      dx = sf(x);
      break;
    case t_longfloat:
      dx = lf(x);
      break;
    default:
      FEwrong_type_argument(Snumber, x);
    }
    if (-1.0 <= dx && dx <= 1.0) return(make_longfloat(asin(dx)));
  }

  /* treat as complex argument, result */
  {
    object r;
    object x0, x1;
    vs_mark;

    x0 = number_times(x, x);
    vs_push(x0);
    x0 = number_minus(small_fixnum(1), x0);
    vs_push(x0);
    x0 = number_sqrt(x0);
    vs_push(x0);
    x1 = number_times(imag_unit, x);
    vs_push(x1);
    x0 = number_plus(x0, x1);
    vs_push(x0);
    x0 = number_nlog(x0);
    vs_push(x0);
    r = number_times(minus_imag_unit, x0);
    vs_reset;
    return(r);
  }
}

object
number_acos(x)
     object x;
{
  double acos();
  double dx;

  /* check for a real argument in [-1,1] */
  if (type_of(x) != t_complex) {
    switch (type_of(x)) {
    case t_fixnum:
    case t_bignum:
    case t_ratio:
      dx = number_to_double(x);
      break;
    case t_shortfloat:
      dx = sf(x);
      break;
    case t_longfloat:
      dx = lf(x);
      break;
    default:
      FEwrong_type_argument(Snumber, x);
    }
    if (-1.0 <= dx && dx <= 1.0) return(make_longfloat(acos(dx)));
  }

  /* treat as complex argument, result */
  {
    object r;
    object x0;
    vs_mark;

    x0 = number_times(x, x);
    vs_push(x0);
    x0 = number_minus(small_fixnum(1), x0);
    vs_push(x0);
    x0 = number_sqrt(x0);
    vs_push(x0);
    x0 = number_times(imag_unit, x0);
    vs_push(x0);
    x0 = number_plus(x0, x);
    vs_push(x0);
    x0 = number_nlog(x0);
    vs_push(x0);
    r = number_times(minus_imag_unit, x0);
    vs_reset;
    return(r);
  }
}

Lexp()
{
	check_arg(1);
	check_type_number(&vs_base[0]);
	vs_base[0] = number_exp(vs_base[0]);
}

Lexpt()
{
	check_arg(2);
	check_type_number(&vs_base[0]);
	check_type_number(&vs_base[1]);
	vs_base[0] = number_expt(vs_base[0], vs_base[1]);
	vs_pop;
}

Llog()
{
	int narg;
	
	narg = vs_top - vs_base;
	if (narg < 1)
		too_few_arguments();
	else if (narg == 1) {
		check_type_number(&vs_base[0]);
		vs_base[0] = number_nlog(vs_base[0]);
	} else if (narg == 2) {
		check_type_number(&vs_base[0]);
		check_type_number(&vs_base[1]);
		vs_base[0] = number_log(vs_base[1], vs_base[0]);
		vs_pop;
	} else
		too_many_arguments();
}

Lsqrt()
{
	check_arg(1);
	check_type_number(&vs_base[0]);
	vs_base[0] = number_sqrt(vs_base[0]);
}

Lsin()
{
	check_arg(1);
	check_type_number(&vs_base[0]);
	vs_base[0] = number_sin(vs_base[0]);
}

Lcos()
{
	check_arg(1);
	check_type_number(&vs_base[0]);
	vs_base[0] = number_cos(vs_base[0]);
}

Ltan()
{
	check_arg(1);
	check_type_number(&vs_base[0]);
	vs_base[0] = number_tan(vs_base[0]);
}

Latan()
{
	int narg;

	narg = vs_top - vs_base;
	if (narg < 1)
		too_few_arguments();
	if (narg == 1) {
		check_type_number(&vs_base[0]);
		vs_base[0] = number_atan(vs_base[0]);
	} else if (narg == 2) {
		check_type_or_rational_float(&vs_base[0]);
		check_type_or_rational_float(&vs_base[1]);
		vs_base[0] = number_atan2(vs_base[0], vs_base[1]);
		vs_pop;
	} else
		too_many_arguments();
}

Lasin()
{
  check_arg(1);
  check_type_number(&vs_base[0]);
  vs_base[0] = number_asin(vs_base[0]);
}

Lacos()
{
  check_arg(1);
  check_type_number(&vs_base[0]);
  vs_base[0] = number_acos(vs_base[0]);
}

init_num_sfun()
{
	imag_unit
	= make_complex(make_longfloat(0.0), make_longfloat(1.0));
	enter_mark_origin(&imag_unit);
	minus_imag_unit
	= make_complex(make_longfloat(0.0), make_longfloat(-1.0));
	enter_mark_origin(&minus_imag_unit);
	imag_two
	= make_complex(make_longfloat(0.0), make_longfloat(2.0));
	enter_mark_origin(&imag_two);

	make_constant("PI", make_longfloat(PI));

	make_function("EXP", Lexp);
	make_function("EXPT", Lexpt);
	make_function("LOG", Llog);
	make_function("SQRT", Lsqrt);
	make_function("SIN", Lsin);
	make_function("COS", Lcos);
	make_function("TAN", Ltan);
	make_function("ATAN", Latan);
	make_function("ASIN", Lasin);
	make_function("ACOS", Lacos);
}
