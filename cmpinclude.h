

/* Begin for cmpinclude */


/* #define SGC */


/* End for cmpinclude */
/*
(c) Copyright Taiichi Yuasa and Masami Hagiya, 1984.  All rights reserved.
Copying of this file is authorized to users who have executed the true and
proper "License Agreement for Kyoto Common LISP" with SIGLISP.
*/
#include <stdio.h>
#include <setjmp.h>
#include <varargs.h>
#define	TRUE	1
#define	FALSE	0
#ifdef SGC
#define FIRSTWORD     short t; char s,m
#define SGC_TOUCH(x) x->d.m=0
#else
#define FIRSTWORD     short t; short m
#define SGC_TOUCH(x)
#endif  
#define STSET(type,x,i,val)  do{SGC_TOUCH(x);STREF(type,x,i) = (val);} while(0)
#ifndef VOL
#define VOL
#endif
#ifndef COM_LENG
#define COM_LENG 
#endif
#ifndef CHAR_SIZE
#define CHAR_SIZE        8     
#endif
typedef int bool;
typedef int fixnum;
typedef float shortfloat;
typedef double longfloat;
typedef  unsigned short fatchar;
#define SIGNED_CHAR(x) (((char ) -1) < (char )0 ? (char) x \
		  : (x >= (1<<(CHAR_SIZE-1)) ? \
		     x - (((int)(1<<(CHAR_SIZE-1))) << 1) \
		     : (char ) x))
typedef union lispunion *object;
typedef union int_object iobject;
union int_object {int i; object o;};

#define	OBJNULL	((object)NULL)
struct fixnum_struct {
		FIRSTWORD;
	fixnum	FIXVAL;
};
#define	fix(x)	(x)->FIX.FIXVAL
#define	SMALL_FIXNUM_LIMIT	1024
extern struct fixnum_struct small_fixnum_table[COM_LENG];
#define	small_fixnum(i)	(object)(small_fixnum_table+SMALL_FIXNUM_LIMIT+(i))

struct bignum {
			FIRSTWORD;
	long             *big_self;	/*  bignum body  */
	int		big_length;	/*  bignum length  */
};
#define MP(x) ((GEN)(x)->big.big_self)
struct shortfloat_struct {
			FIRSTWORD;
	shortfloat	SFVAL;
};
#define	sf(x)	(x)->SF.SFVAL
struct longfloat_struct {
			FIRSTWORD;
	longfloat	LFVAL;
};
#define	lf(x)	(x)->LF.LFVAL
struct character {
			FIRSTWORD;
	unsigned short	ch_code;
	unsigned char	ch_font;
	unsigned char	ch_bits;
};
struct character character_table1[256+128];
#define character_table (character_table1+128)
#define	code_char(c)	(object)(character_table+(c))
#define	char_code(x)	(x)->ch.ch_code
#define	char_font(x)	(x)->ch.ch_font
#define	char_bits(x)	(x)->ch.ch_bits
enum stype {
	stp_ordinary,
	stp_constant,
        stp_special
};
struct symbol {
		FIRSTWORD;
	object	s_dbind;
	int	(*s_sfdef)();
#define	s_fillp		st_fillp
#define	s_self		st_self
	int	s_fillp;
	char	*s_self;
	object	s_gfdef;
	object	s_plist;
	object	s_hpack;
	short	s_stype;
	short	s_mflag;
};
struct cons {
		FIRSTWORD;
	object	c_cdr;
	object	c_car;
};
struct array {
		FIRSTWORD;
	short	a_rank;
	short	a_adjustable;
	int	a_dim;
	int	*a_dims;
	object	*a_self;
	object	a_displaced;
	short	a_elttype;
	short	a_offset;
};



struct fat_string {			/*  vector header  */
		FIRSTWORD;
        unsigned fs_raw : 24;     /* tells if the things in leader are raw */
	unsigned char fs_leader_length;	 /* leader_Length  */
	int	fs_dim;		/*  dimension  */
	int	fs_fillp;	/*  fill pointer  */
				/*  For simple vectors,  */
				/*  fs_fillp is equal to fs_dim.  */
	fatchar 	*fs_self;	/*  pointer to the vector Note the leader starts at (int *) *fs_self - fs_leader_length */
};


struct vector {
		FIRSTWORD;
	short	v_hasfillp;
	short	v_adjustable;
	int	v_dim;
	int	v_fillp;
	object	*v_self;
	object	v_displaced;
	short	v_elttype;
	short	v_offset;
};
struct string {
		FIRSTWORD;
	short	st_hasfillp;
	short	st_adjustable;
	int	st_dim;
	int	st_fillp;
	char	*st_self;
	object	st_displaced;
};
struct ustring {
		FIRSTWORD;
	short	ust_hasfillp;
	short	ust_adjustable;
	int	ust_dim;
	int	ust_fillp;
	unsigned char
		*ust_self;
	object	ust_displaced;
};
#define USHORT(x,i) (((unsigned short *)(x)->ust.ust_self)[i])

struct bitvector {
		FIRSTWORD;
	short	bv_hasfillp;
	short	bv_adjustable;
	int	bv_dim;
	int	bv_fillp;
	char	*bv_self;
	object	bv_displaced;
	short	bv_elttype;
	short	bv_offset;
};
struct fixarray {
		FIRSTWORD;
	short	fixa_rank;
	short	fixa_adjustable;
	int	fixa_dim;
	int	*fixa_dims;
	fixnum	*fixa_self;
	object	fixa_displaced;
	short	fixa_elttype;
	short	fixa_offset;
};
struct sfarray {
		FIRSTWORD;
	short	sfa_rank;
	short	sfa_adjustable;
	int	sfa_dim;
	int	*sfa_dims;
	shortfloat
		*sfa_self;
	object	sfa_displaced;
	short	sfa_elttype;
	short	sfa_offset;
};
struct lfarray {
		FIRSTWORD;
	short	lfa_rank;
	short	lfa_adjustable;
	int	lfa_dim;
	int	*lfa_dims;
	longfloat
		*lfa_self;
	object	lfa_displaced;
	short	lfa_elttype;
	short	lfa_offset;
};

struct structure {		/*  structure header  */
		FIRSTWORD;
	object	str_def;	/*  structure definition (a structure)  */
	object	*str_self;	/*  structure self  */
};

#define STREF(type,x,i) (*((type *)(((char *)((x)->str.str_self))+(i))))

struct cfun {
		FIRSTWORD;
	object	cf_name;
	int	(*cf_self)();
	object	cf_data;
};

  struct dclosure {		/*  compiled closure header  */
		FIRSTWORD;
	int	(*dc_self)();	/*  entry address  */
	object	*dc_env;	/*  environment  */
};

  struct cclosure {
		FIRSTWORD;

	object	cc_name;
	int	(*cc_self)();
	object	cc_env;
	object	cc_data;
	object	*cc_turbo;
};

struct sfun {
	FIRSTWORD;
	object	sfn_name;
	int	(*sfn_self)();
	object	sfn_data;
	int sfn_argd;

	      };
struct vfun {
		FIRSTWORD; 
	object	vfn_name;
	int	(*vfn_self)();
	object	vfn_data;
	unsigned short vfn_minargs;
	unsigned short vfn_maxargs;
	      };

struct dummy {
		FIRSTWORD;
};
struct stream {
		FIRSTWORD;
	FILE	*sm_fp;		/*  file pointer  */
	object	sm_object0;	/*  some object  */
	object	sm_object1;	/*  some object */
	int	sm_int0;	/*  some int  */
	int	sm_int1;	/*  some int  */
	char  	*sm_buffer;     /*  ptr to BUFSIZE block of storage */
	short	sm_mode;	/*  stream mode  */
				/*  of enum smmode  */
};
union lispunion {
	struct fixnum_struct
			FIX;
	struct shortfloat_struct
			SF;
	struct stream sm;
	struct longfloat_struct
			LF;
	struct character
			ch;
	struct symbol	s;
	struct cons	c;
	struct array	a;
	struct vector	v;
	struct string	st;
	struct ustring	ust;
	struct bignum   big;
	struct bitvector
			bv;
	struct structure
			str;
	struct cfun	cf;
	struct cclosure	cc;
	struct sfun     sfn;
	struct vfun     vfn;
	struct dummy	d;
        struct fat_string fs;
        struct dclosure dc;
	struct fixarray	fixa;
	struct sfarray	sfa;
	struct lfarray	lfa;
};
enum type {
	t_cons,
	t_start = 0 , /* t_cons, */
	t_fixnum,
	t_bignum,
	t_ratio,
	t_shortfloat,
	t_longfloat,
	t_complex,
	t_character,
	t_symbol,
	t_package,
	t_hashtable,
	t_array,
	t_vector,
	t_string,
	t_bitvector,
	t_structure,
	t_stream,
	t_random,
	t_readtable,
	t_pathname,
	t_cfun,
	t_cclosure,
	t_sfun,
        t_gfun,
	t_vfun,
	t_cfdata,
	t_spice,
	t_fat_string,
        t_dclosure,
	t_end,
	t_contiguous,
	t_relocatable,
	t_other
};
#define	type_of(obje)	((enum type)(((object)(obje))->d.t))
#define	endp(obje)	endp1(obje)
extern object value_stack[COM_LENG];
#define	vs_org		value_stack
object *vs_limit;
object *vs_base;
object *vs_top;
#define	vs_push(obje)	(*vs_top++ = (obje))
#define	vs_pop		(*--vs_top)
#define	vs_head		vs_top[-1]
#define	vs_mark		object *old_vs_top = vs_top
#define	vs_reset	vs_top = old_vs_top
#define	vs_check	if (vs_top >= vs_limit)  \
				vs_overflow();
#define	vs_check_push(obje)  \
			(vs_top >= vs_limit ?  \
			 (object)vs_overflow() : (*vs_top++ = (obje)))
#define	check_arg(n)  \
			if (vs_top - vs_base != (n))  \
				check_arg_failed(n)
#define	MMcheck_arg(n)  \
			if (vs_top - vs_base < (n))  \
				too_few_arguments();  \
			else if (vs_top - vs_base > (n))  \
				too_many_arguments()
#define vs_reserve(x)	if(vs_base+(x) >= vs_limit)  \
				vs_overflow();
struct bds_bd {
	object	bds_sym;
	object	bds_val;
};
extern struct bds_bd bind_stack[COM_LENG];
typedef struct bds_bd *bds_ptr;
bds_ptr bds_org;
bds_ptr bds_limit;
bds_ptr bds_top;
#define	bds_check  \
	if (bds_top >= bds_limit)  \
		bds_overflow()
#define	bds_bind(sym, val)  \
	((++bds_top)->bds_sym = (sym),  \
	bds_top->bds_val = (sym)->s.s_dbind,  \
	(sym)->s.s_dbind = (val))
#define	bds_unwind1  \
	((bds_top->bds_sym)->s.s_dbind = bds_top->bds_val, --bds_top)
typedef struct invocation_history {
	object	ihs_function;
	object	*ihs_base;
} *ihs_ptr;
extern struct invocation_history ihs_stack[COM_LENG];
ihs_ptr ihs_org;
ihs_ptr ihs_limit;
ihs_ptr ihs_top;
#define	ihs_check  \
	if (ihs_top >= ihs_limit)  \
		ihs_overflow()
#define ihs_push(function)  \
	(++ihs_top)->ihs_function = (function);  \
	ihs_top->ihs_base = vs_base
#define ihs_pop() 	(ihs_top--)
enum fr_class {
	FRS_CATCH,
	FRS_CATCHALL,
	FRS_PROTECT
};
struct frame {
	jmp_buf		frs_jmpbuf;
	object		*frs_lex;
	bds_ptr		frs_bds_top;
	enum fr_class	frs_class;
	object		frs_val;
	ihs_ptr		frs_ihs;
};
typedef struct frame *frame_ptr;
#define	alloc_frame_id()	alloc_object(t_spice)
extern struct frame frame_stack[COM_LENG];

frame_ptr frs_org;
frame_ptr frs_limit;
frame_ptr frs_top;
#define frs_push(class, val)  \
	if (++frs_top >= frs_limit)  \
		frs_overflow();  \
	frs_top->frs_lex = lex_env;\
	frs_top->frs_bds_top = bds_top;  \
	frs_top->frs_class = (class);  \
	frs_top->frs_val = (val);  \
	frs_top->frs_ihs = ihs_top;  \
        setjmp(frs_top->frs_jmpbuf)
#define frs_pop()	frs_top--
bool nlj_active;
frame_ptr nlj_fr;
object nlj_tag;
object *lex_env;
object caar();
object cadr();
object cdar();
object cddr();
object caaar();
object caadr();
object cadar();
object caddr();
object cdaar();
object cdadr();
object cddar();
object cdddr();
object caaaar();
object caaadr();
object caadar();
object caaddr();
object cadaar();
object cadadr();
object caddar();
object cadddr();
object cdaaar();
object cdaadr();
object cdadar();
object cdaddr();
object cddaar();
object cddadr();
object cdddar();
object cddddr();
#define MMcons(a,d)	make_cons((a),(d))
#define MMcar(x)	(x)->c.c_car
#define MMcdr(x)	(x)->c.c_cdr
#define CMPcar(x)	(x)->c.c_car
#define CMPcdr(x)	(x)->c.c_cdr
#define CMPcaar(x)	(x)->c.c_car->c.c_car
#define CMPcadr(x)	(x)->c.c_cdr->c.c_car
#define CMPcdar(x)	(x)->c.c_car->c.c_cdr
#define CMPcddr(x)	(x)->c.c_cdr->c.c_cdr
#define CMPcaaar(x)	(x)->c.c_car->c.c_car->c.c_car
#define CMPcaadr(x)	(x)->c.c_cdr->c.c_car->c.c_car
#define CMPcadar(x)	(x)->c.c_car->c.c_cdr->c.c_car
#define CMPcaddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_car
#define CMPcdaar(x)	(x)->c.c_car->c.c_car->c.c_cdr
#define CMPcdadr(x)	(x)->c.c_cdr->c.c_car->c.c_cdr
#define CMPcddar(x)	(x)->c.c_car->c.c_cdr->c.c_cdr
#define CMPcdddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_cdr
#define CMPcaaaar(x)	(x)->c.c_car->c.c_car->c.c_car->c.c_car
#define CMPcaaadr(x)	(x)->c.c_cdr->c.c_car->c.c_car->c.c_car
#define CMPcaadar(x)	(x)->c.c_car->c.c_cdr->c.c_car->c.c_car
#define CMPcaaddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_car->c.c_car
#define CMPcadaar(x)	(x)->c.c_car->c.c_car->c.c_cdr->c.c_car
#define CMPcadadr(x)	(x)->c.c_cdr->c.c_car->c.c_cdr->c.c_car
#define CMPcaddar(x)	(x)->c.c_car->c.c_cdr->c.c_cdr->c.c_car
#define CMPcadddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_cdr->c.c_car
#define CMPcdaaar(x)	(x)->c.c_car->c.c_car->c.c_car->c.c_cdr
#define CMPcdaadr(x)	(x)->c.c_cdr->c.c_car->c.c_car->c.c_cdr
#define CMPcdadar(x)	(x)->c.c_car->c.c_cdr->c.c_car->c.c_cdr
#define CMPcdaddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_car->c.c_cdr
#define CMPcddaar(x)	(x)->c.c_car->c.c_car->c.c_cdr->c.c_cdr
#define CMPcddadr(x)	(x)->c.c_cdr->c.c_car->c.c_cdr->c.c_cdr
#define CMPcdddar(x)	(x)->c.c_car->c.c_cdr->c.c_cdr->c.c_cdr
#define CMPcddddr(x)	(x)->c.c_cdr->c.c_cdr->c.c_cdr->c.c_cdr
#define CMPfuncall	funcall
#define	cclosure_call	funcall
object simple_lispcall();
object simple_lispcall_no_event();
object simple_symlispcall();
object simple_symlispcall_no_event();
object CMPtemp;
object CMPtemp1;
object CMPtemp2;
object CMPtemp3;
#define	Cnil	((object)&Cnil_body)
#define	Ct	((object)&Ct_body)
struct symbol Cnil_body, Ct_body;
object MF();
object MFnew();
object MM();
object Scons;
object siSfunction_documentation;
object siSvariable_documentation;
object siSpretty_print_format;
object Slist;
object keyword_package;
object alloc_object();
object car();
object cdr();
object list();
object listA();
object coerce_to_string();
object elt();
object elt_set();
frame_ptr frs_sch();
frame_ptr frs_sch_catch();
object make_cclosure();
object make_cclosure_new();
object nth();
object nthcdr();
object make_cons();
object append();
object nconc();
object reverse();
object nreverse();
object number_expt();
object number_minus();
object number_negate();
object number_plus();
object number_times();
object one_minus();
object one_plus();
object get();
object getf();
object putprop();
object sputprop();
object remprop();
object string_to_object();
object symbol_function();
object symbol_value();
object make_fixnum();
object make_shortfloat();
object make_longfloat();
object structure_ref();
object structure_set();
object princ();
object prin1();
object print();
object terpri();
object aref();
object aset();
object aref1();
object aset1();
void call_or_link();
object call_proc();
object call_proc0();
object call_proc1();
object call_proc2();
object ifuncall();   
object ifuncall1();
object ifuncall2();
object symbol_name();
char object_to_char();
int object_to_int();
float object_to_float();
double object_to_double();
char *object_to_string();
int FIXtemp;
#define	CMPmake_fixnum(x) \
((((FIXtemp=(x))+1024)&-2048)==0?small_fixnum(FIXtemp):make_fixnum(FIXtemp))
#define Creturn(v) return((vs_top=vs,(v)))
#define Cexit return((vs_top=vs,0))
double sin(), cos(), tan();
object read_byte1(),read_char1();

#define fs_leader(ar,i) (((object *)((ar)->fs.fs_self))[-(i+1)])
#define RPAREN )
object make_list();
#ifdef HAVE_ALLOCA
#ifndef alloca
char *alloca();
#endif
char *alloca_val;
#define ALLOCA_CONS(n) (alloca_val=alloca((n)*sizeof(struct cons))) 
#define ON_STACK_CONS(x,y) (alloca_val=alloca(sizeof(struct cons)), on_stack_cons(x,y)) 
#define ON_STACK_LIST on_stack_list
#define ON_STACK_LIST_VECTOR on_stack_list_vector
#define ON_STACK_MAKE_LIST on_stack_make_list
object on_stack_cons();
object on_stack_list();
object on_stack_list_vector();
object on_stack_make_list();
#else
#define ALLOCA_CONS(n) 0
#define ON_STACK_CONS(x,y) MMcons(x,y)
#define ON_STACK_LIST list
#define ON_STACK_LIST_VECTOR list_vector
#define ON_STACK_MAKE_LIST make_list
#endif


struct call_data { object fun;
		   int argd;};
struct call_data fcall;
object  fcalln();
object list_vector();
object MVloc[10];
#define VARG(min,max) ((min) | (max << 8))
#define  VFUN_NARGS fcall.argd
extern object Cstd_key_defaults[];
int vfun_wrong_number_of_args();
int eql(),equal(),eq();
object sublis1();
object LVformat(),LVerror();
#define EQ(x,y) ((x)==(y))



/* #include "../h/genpari.h"*/
typedef unsigned long *GEN;
GEN addii(),mulii(),mulsi(),powerii(),shifti(),stoi(),dvmdii(),subii();
int cmpii();
#define signe(x)          (((GEN)(x))[1]>>24)
#define lg(x)             (((GEN)(x))[0]&0xffff)
#define setlg(x,s)        (((GEN)(x))[0]=(((GEN)(x))[0]&0xffff0000)+s)
#define lgef(x)           (((GEN)(x))[1]&0xffff)
#define setlgef(x,s)      (((GEN)(x))[1]=(((GEN)(x))[1]&0xffff0000)+s)

int in_saved_avma ;
#define ulong unsigned long
/* #define DEBUG_AVMA */

#ifdef DEBUG_AVMA
#define save_avma long lvma = (in_saved_avma = 1, avma)
#define restore_avma avma = (in_saved_avma = 0, lvma)
#else
#define save_avma long lvma = avma
#define restore_avma avma = lvma
#endif
unsigned long avma;
GEN gzero;
GEN icopy_x;

object make_integer();
  /* copy x to y, increasing space by factor of 2  */


GEN otoi();
/*
object integ_temp;
#define otoi(x) (integ_temp = (x) , (type_of(integ_temp) == t_bignum \
   ? MP(integ_temp) :stoi(fix(integ_temp))))
*/

void isetq_fix();
#ifdef HAVE_ALLOCA
#define SETQ_II(var,alloc,val) \
  do{GEN _xx =(val) ; \
  int _n = replace_copy1(_xx,var); \
  if(_n) var = replace_copy2(_xx,alloca(_n));}while(0)

#define SETQ_IO(var,alloc,val) {object _xx =(val) ; \
			      int _n = obj_replace_copy1(_xx,var); \
			    if(_n) var = obj_replace_copy2(_xx,alloca(_n));}
#define IDECL(a,b,c) ulong b[4];a =(b[0]=0x1010000 +4,b)
#else
GEN setq_io(),setq_ii();
#define SETQ_IO(x,alloc,val)   (x)=setq_io(x,&alloc,val)
#define SETQ_II(x,alloc,val)   (x)=setq_ii(x,&alloc,val)
#define IDECL(a,b,c) ulong b[4];a =(b[0]=0x1010000 +4,b);object c
#endif


#ifdef __GNUC__
#define alloca __builtin_alloca
#endif


