#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

extern "C"
{
#define Alloc(x, type, size)			     \
    do { PROTECT((x) = allocVector((type), (size))); \
	stacksize++;				     \
    } while (0)
#define INT2SEXP(x, y, n)			\
    do { Alloc((y), INTSXP, (n));		\
	{ int i;				\
	    for (i = 0; i < (n); i++)		\
		INTEGER((y))[i] = (x)[i];	\
	}					\
    } while (0)
#define REAL2SEXP(x, y, n)			\
    do { Alloc((y), REALSXP, (n));		\
	{ int i;				\
	    for (i = 0; i < (n); i++)		\
		REAL((y))[i] = (x)[i];		\
	}					\
    } while (0)
#define SAMPLE(i) (int)ftrunc((i)*unif_rand())
#define TR(i,j)					    \
    t[0][i]*t[0][j]*pr[0] + t[0][i]*t[1][j]*pr[1] + \
    t[1][i]*t[0][j]*pr[2] + t[1][i]*t[1][j]*pr[3]
#define TL(i,j)					    \
    t[0][i]*t[0][j]*pl[0] + t[0][i]*t[1][j]*pl[1] + \
    t[1][i]*t[0][j]*pl[2] + t[1][i]*t[1][j]*pl[3]
#define TU(i,j)					    \
    t[0][i]*t[0][j]*pu[0] + t[0][i]*t[1][j]*pu[1] + \
    t[1][i]*t[0][j]*pu[2] + t[1][i]*t[1][j]*pu[3]

static void make_cdf   (double*, double*, int);
static int  make_vector(SEXP _vec, SEXP _elt, ...);
static int  make_names (SEXP _names, const char* name, ...);

SEXP _genprior_c (SEXP _n, SEXP _alpha, 
		  SEXP _fdr, SEXP _ado, SEXP _Blin, SEXP _Bmut)
{
    /*number of samples*/
    int n = INTEGER(_n)[0];
    /*lead time factor alpha*/
    double alpha = REAL(_alpha)[0];
    /*total number of lineages generated*/
    int Blin = INTEGER(_Blin)[0];
    /*total number of mutation pairs given lineage generated*/
    int Bmut = INTEGER(_Bmut)[0];

    if (n < 2 || n > 58)
	error("n should be between 2 and 58\n", n);
    if (alpha < 0 || alpha >= 1)
	error("alpha should be in [0,1)\n");

    /*total number of mutations given lineage*/
    if (Bmut < 1)
	error("arg Bmut was %d, should be greater than 1\n", Bmut);

    int    *fm       = Calloc(n*n, int);
    int    *bm       = Calloc(n*n, int);
    int    *brPos    = Calloc(n-1, int);
    double *T_exp    = Calloc(n, double);
    double *brDepCDF = Calloc(n, double);

    int i, j;
    for (i = 0; i < n; i++)
	for (j = 0; j < n; j++)
	    fm[i + n*j] = bm[i + n*j] = -1;
    
    int *ldep = Calloc(Bmut*2, int);
    int *lbrc = Calloc(Bmut*2, int);
    int *oc   = Calloc(Bmut*2*n, int);

    int ltt, lww, lwm, lmw, lmm, utt, uww, uwm, umw, umm;
    int ww, wm, mw, mm, pos, ij;
    double mut;
    ltt = lww = lwm = lmw = lmm = 0;
    utt = uww = uwm = umw = umm = 0;

    GetRNGstate();

    int b;
    for (b = 0; b < Blin; b++)
    {
	for (i = 0; i < n-1; i++)
	    brPos[i] = SAMPLE(i);

	/* forward split */
	fm[0] = 0;
	for (j = 0; j < n-1; j++) {
	    for (i = 0; i <= j; i++)
		if (brPos[j] == fm[i + n*j]) 
		    pos = i;
	    for (i = 0; i <= pos; i++)
		fm[i + n*(j+1)] = fm[i + n*j];
	    fm[pos+1 + n*(j+1)] = j+1;
	    if (pos < j)
		for (i = pos+2; i <= j+1; i++)
		    fm[i + n*(j+1)] = fm[i-1 + n*j];
	}

	/* backward merge */
	for (i = 0; i < n; i++)
	    bm[i + n*(n-1)] = fm[i + n*(n-1)];
	for (j = n-1; j > 0; j--) {
	    i = 0;
	    while (bm[i + n*j] != j)
		i++;
	    pos = i;
	    for (i = 0; i < n; i++)
		if (j == bm[i + n*j])
		    bm[i + n*(j-1)] = bm[pos-1 + n*j];
		else
		    bm[i + n*(j-1)] = bm[i + n*j];
	}

	for (i = 1; i < n; i++)
	    T_exp[i] = -2.*log(unif_rand())/i/(i+1);

	double sum = 0.0;
	for (i = 1; i < n; i++)
	    sum += T_exp[i];

	T_exp[0] = alpha/(1 - alpha)*sum;

	make_cdf(T_exp, brDepCDF, n);

	for (i = 0; i < Bmut*2; i++) {
	    mut = unif_rand();
	    j = 0;
	    while (mut > brDepCDF[j])
		j++;
	    ldep[i] = j;
	    lbrc[i] = SAMPLE(j);
	}

	for (i = 0; i < n; i++)
	    for (j = 0; j < Bmut*2; j++) {
		oc[i + n*j] = 0;
		if (bm[i + n*ldep[j]] == lbrc[j])
		    oc[i + n*j] = 1;
	    }

	for (j = 0; j < Bmut; j++) {

	    ww = wm = mw = mm = 0;
	    for (i = 0; i < n; i++) {

		ij = oc[i + n*j]*2 + oc[i + n*(j+Bmut)];

		switch(ij) {
		case 0:
		    ww++; break;
		case 1:
		    wm++; break;
		case 2:
		    mw++; break;
		case 3:
		    mm++; break;
		default:
		    error("ij should be one of 0,1,2,3\n");
		}
	    }
	    if (mm > 0) {
		ltt++;
		lww += ww;
		lwm += wm;
		lmw += mw;
		lmm += mm;
	    } else {
		utt++;
		uww += ww;
		uwm += wm;
		umw += mw;
		umm += mm;
	    }		
	}
    }

    PutRNGstate();
    
    if (ltt + utt != Blin*Bmut)
	error("ltt + utt != Blin*Bmut\n");
    if (umm != 0)
	error("umm != 0\n");
    if (ltt*n != lww + lwm + lmw + lmm)
	error("ltt*n != lww + lwm + lmw + lmm\n");
    if (utt*n != uww + uwm + umw + umm)
	error("utt*n != uww + uwm + umw + umm\n");

    Free(fm);
    Free(bm);
    Free(brPos);
    Free(T_exp);
    Free(brDepCDF);
    Free(ldep);
    Free(lbrc);
    Free(oc);

    double tp[3] = {0.0};
    double pr[4] = {0.0};
    double pl[4] = {0.0};
    double pu[4] = {0.0};
    if (ltt > 0) {
	tp[0] = (double)ltt/Blin/Bmut/2.;
	pr[0] = (double)lww/ltt/n;
	pr[1] = 0.0;
	pr[2] = (double)lmw/ltt/n + (double)lwm/ltt/n;
	pr[3] = (double)lmm/ltt/n;

	tp[1] = tp[0];
	pl[0] = pr[0];
	pl[1] = pr[2];
	pl[2] = pr[1];
	pl[3] = pr[3];
    }

    if (utt > 0) {
	tp[2] = (double)utt/Blin/Bmut;
	pu[0] = (double)uww/utt/n;
	pu[1] = (double)uwm/utt/n;
	pu[2] = (double)umw/utt/n;
	pu[3] = (double)umm/utt/n;
    }

    double fdr = REAL(_fdr)[0];
    double ado = REAL(_ado)[0];
    double fdr2 = fdr*ado/2; /* true 0 -> observed 2 */
    double t[2][3] = {{1-fdr-fdr2, fdr, fdr2}, {ado/2, 1-ado, ado/2}};
    double qr[9] = {0.0};
    double ql[9] = {0.0};
    double qu[9] = {0.0};
    double mp[9] = {0.0};

    for (i = 0; i < 3; i++)
	for (j = 0; j < 3; j++) {
	    int ij = i*3 + j;
	    qr[ij] = TR(i,j);
	    ql[ij] = TL(i,j);
	    qu[ij] = TU(i,j);
	    mp[ij] = qr[ij]*tp[0] + ql[ij]*tp[1] + qu[ij]*tp[2];
	}

    int stacksize = 0;

    SEXP _tp, _pr, _pl, _pu, _qr, _ql, _qu, _mp;
    REAL2SEXP(tp, _tp, 3);
    REAL2SEXP(pr, _pr, 4);
    REAL2SEXP(pl, _pl, 4);
    REAL2SEXP(pu, _pu, 4);
    REAL2SEXP(qr, _qr, 9);
    REAL2SEXP(ql, _ql, 9);
    REAL2SEXP(qu, _qu, 9);
    REAL2SEXP(mp, _mp, 9);

    SEXP _res, _names;
    Alloc(_res, VECSXP, 100);
    Alloc(_names, STRSXP, 100);
    make_vector(_res, _tp, _pr, _pl, _pu, _qr, _ql, _qu, _mp,
		_alpha, _fdr, _ado, NULL);
    make_names(_names, "tp", "pr", "pl", "pu", "qr", "ql", "qu", "mp",
	       "alpha", "fdr", "ado", NULL);
    setAttrib(_res, R_NamesSymbol, _names);

    UNPROTECT(stacksize);
    return _res;
}

SEXP _genprior_v (SEXP _n, SEXP _alpha, SEXP _rho, SEXP _vpercent,
		  SEXP _fdr, SEXP _ado, SEXP _Blin, SEXP _Bmut)
{
    /*number of samples*/
    int n = INTEGER(_n)[0];
    /*lead time factor alpha*/
    double alpha = REAL(_alpha)[0];
    /*varying population size */
    double rho = REAL(_rho)[0];
    /*the point where population size stays constant*/
    double vpercent = REAL(_vpercent)[0];
    /*total number of lineages generated*/
    int Blin = INTEGER(_Blin)[0];
    /*total number of mutation pairs given lineage generated*/
    int Bmut = INTEGER(_Bmut)[0];

    if (n < 2 || n > 58)
	error("n should be between 2 and 58\n", n);
    if (alpha < 0 || alpha >= 1)
	error("alpha should be in [0,1)\n");
    if (vpercent < 0 || vpercent > 100)
	error("vpercent should be in [0,100]\n");
    if (rho < 0)
	error("rho should be non-negative\n");

    /*total number of mutations given lineage*/
    if (Bmut < 1)
	error("arg Bmut was %d, should be greater than 1\n", Bmut);

    int    *fm       = Calloc(n*n, int);
    int    *bm       = Calloc(n*n, int);
    int    *brPos    = Calloc(n-1, int);
    double *T_exp    = Calloc(n, double);
    double *T_var    = Calloc(n, double);
    double *S_exp    = Calloc(n, double);
    double *S_var    = Calloc(n, double);
    double *brDepCDF = Calloc(n, double);

    int i, j;
    for (i = 0; i < n; i++)
	for (j = 0; j < n; j++)
	    fm[i + n*j] = bm[i + n*j] = -1;
    
    int *ldep = Calloc(Bmut*2, int);
    int *lbrc = Calloc(Bmut*2, int);
    int *oc   = Calloc(Bmut*2*n, int);

    int ltt, lww, lwm, lmw, lmm, utt, uww, uwm, umw, umm;
    int ww, wm, mw, mm, pos, ij;
    double mut;
    ltt = lww = lwm = lmw = lmm = 0;
    utt = uww = uwm = umw = umm = 0;

    GetRNGstate();

    int b;
    for (b = 0; b < Blin; b++)
    {
	for (i = 0; i < n-1; i++)
	    brPos[i] = SAMPLE(i);

	/* forward split */
	fm[0] = 0;
	for (j = 0; j < n-1; j++) {
	    for (i = 0; i <= j; i++)
		if (brPos[j] == fm[i + n*j]) 
		    pos = i;
	    for (i = 0; i <= pos; i++)
		fm[i + n*(j+1)] = fm[i + n*j];
	    fm[pos+1 + n*(j+1)] = j+1;
	    if (pos < j)
		for (i = pos+2; i <= j+1; i++)
		    fm[i + n*(j+1)] = fm[i-1 + n*j];
	}

	/* backward merge */
	for (i = 0; i < n; i++)
	    bm[i + n*(n-1)] = fm[i + n*(n-1)];
	for (j = n-1; j > 0; j--) {
	    i = 0;
	    while (bm[i + n*j] != j)
		i++;
	    pos = i;
	    for (i = 0; i < n; i++)
		if (j == bm[i + n*j])
		    bm[i + n*(j-1)] = bm[pos-1 + n*j];
		else
		    bm[i + n*(j-1)] = bm[i + n*j];
	}

	for (i = 1; i < n; i++)
	    T_exp[i] = -2.*log(unif_rand())/i/(i+1);

	for (i = 1; i < n; i++) {
	    S_exp[i] = 0.0;
	    for (j = i; j < n; j++)
		S_exp[i] += T_exp[j];
	}
	T_exp[0] = alpha/(1 - alpha)*S_exp[1];
	S_exp[0] = T_exp[0] + S_exp[1];

	if (rho > 0.0) {
	    double v = S_exp[0]*vpercent/100.;
	    if (v > 0.0) {
		double Lambda_v = v/rho*(exp(rho) - 1.);
		for (i = 0; i < n; i++) 
		    if (S_exp[i] > Lambda_v)
			S_var[i] = exp(-rho)*(S_exp[i] - v/rho*(exp(rho) - 1.)) + v;
		    else
			S_var[i] = v/rho*log(rho/v*S_exp[i] + 1.);
	    } else {
		for (i = 0; i < n; i++) 
		    S_var[i] = exp(-rho)*S_exp[i];
	    }
	} else { /* rho == 0.0 means constant population size */
	    for (i = 0; i < n; i++) 
		S_var[i] = S_exp[i];
	}

	for (i = 0; i < n-1; i++)
	    T_var[i] = S_var[i] - S_var[i+1];
	T_var[n-1] = S_var[n-1];

	make_cdf(T_var, brDepCDF, n);
	/* make_cdf(T_exp, brDepCDF, n); */

	for (i = 0; i < Bmut*2; i++) {
	    mut = unif_rand();
	    j = 0;
	    while (mut > brDepCDF[j])
		j++;
	    ldep[i] = j;
	    lbrc[i] = SAMPLE(j);
	}

	for (i = 0; i < n; i++)
	    for (j = 0; j < Bmut*2; j++) {
		oc[i + n*j] = 0;
		if (bm[i + n*ldep[j]] == lbrc[j])
		    oc[i + n*j] = 1;
	    }

	for (j = 0; j < Bmut; j++) {

	    ww = wm = mw = mm = 0;
	    for (i = 0; i < n; i++) {

		ij = oc[i + n*j]*2 + oc[i + n*(j+Bmut)];

		switch(ij) {
		case 0:
		    ww++; break;
		case 1:
		    wm++; break;
		case 2:
		    mw++; break;
		case 3:
		    mm++; break;
		default:
		    error("ij should be one of 0,1,2,3\n");
		}
	    }
	    if (mm > 0) {
		ltt++;
		lww += ww;
		lwm += wm;
		lmw += mw;
		lmm += mm;
	    } else {
		utt++;
		uww += ww;
		uwm += wm;
		umw += mw;
		umm += mm;
	    }		
	}
    }

    PutRNGstate();
    
    if (ltt + utt != Blin*Bmut)
	error("ltt + utt != Blin*Bmut\n");
    if (umm != 0)
	error("umm != 0\n");
    if (ltt*n != lww + lwm + lmw + lmm)
	error("ltt*n != lww + lwm + lmw + lmm\n");
    if (utt*n != uww + uwm + umw + umm)
	error("utt*n != uww + uwm + umw + umm\n");

    Free(fm);
    Free(bm);
    Free(brPos);
    Free(T_exp);
    Free(T_var);
    Free(S_exp);
    Free(S_var);
    Free(brDepCDF);
    Free(ldep);
    Free(lbrc);
    Free(oc);

    double tp[3] = {0.0};
    double pr[4] = {0.0};
    double pl[4] = {0.0};
    double pu[4] = {0.0};
    if (ltt > 0) {
	tp[0] = (double)ltt/Blin/Bmut/2.;
	pr[0] = (double)lww/ltt/n;
	pr[1] = 0.0;
	pr[2] = (double)lmw/ltt/n + (double)lwm/ltt/n;
	pr[3] = (double)lmm/ltt/n;

	tp[1] = tp[0];
	pl[0] = pr[0];
	pl[1] = pr[2];
	pl[2] = pr[1];
	pl[3] = pr[3];
    }

    if (utt > 0) {
	tp[2] = (double)utt/Blin/Bmut;
	pu[0] = (double)uww/utt/n;
	pu[1] = (double)uwm/utt/n;
	pu[2] = (double)umw/utt/n;
	pu[3] = (double)umm/utt/n;
    }

    double fdr = REAL(_fdr)[0];
    double ado = REAL(_ado)[0];
    double fdr2 = fdr*ado/2; /* true 0 -> observed 2 */
    double t[2][3] = {{1-fdr-fdr2, fdr, fdr2}, {ado/2, 1-ado, ado/2}};
    double qr[9] = {0.0};
    double ql[9] = {0.0};
    double qu[9] = {0.0};
    double mp[9] = {0.0};

    for (i = 0; i < 3; i++)
	for (j = 0; j < 3; j++) {
	    int ij = i*3 + j;
	    qr[ij] = TR(i,j);
	    ql[ij] = TL(i,j);
	    qu[ij] = TU(i,j);
	    mp[ij] = qr[ij]*tp[0] + ql[ij]*tp[1] + qu[ij]*tp[2];
	}

    int stacksize = 0;

    SEXP _tp, _pr, _pl, _pu, _qr, _ql, _qu, _mp;
    REAL2SEXP(tp, _tp, 3);
    REAL2SEXP(pr, _pr, 4);
    REAL2SEXP(pl, _pl, 4);
    REAL2SEXP(pu, _pu, 4);
    REAL2SEXP(qr, _qr, 9);
    REAL2SEXP(ql, _ql, 9);
    REAL2SEXP(qu, _qu, 9);
    REAL2SEXP(mp, _mp, 9);

    SEXP _res, _names;
    Alloc(_res, VECSXP, 100);
    Alloc(_names, STRSXP, 100);
    make_vector(_res, _tp, _pr, _pl, _pu, _qr, _ql, _qu, _mp,
		_alpha, _rho, _vpercent,  _fdr, _ado, NULL);
    make_names(_names, "tp", "pr", "pl", "pu", "qr", "ql", "qu", "mp",
	       "alpha", "rho", "vpercent", "fdr", "ado", NULL);
    setAttrib(_res, R_NamesSymbol, _names);

    UNPROTECT(stacksize);
    return _res;
}

static void make_cdf(double* x, double* y, int n)
{
    int i;
    for (i = 0; i < n; i++) y[i] = x[i]*(i + 1.);
    
    double sum = 0.0;
    for (i = 0; i < n; i++) sum += y[i];
    for (i = 1; i < n; i++) y[i] = y[i] + y[i-1];
    for (i = 0; i < n; i++) y[i] = y[i]/sum;
}

static int make_vector(SEXP _vec, SEXP _elt, ...)
{
    int i = 0;
    va_list ap;
    va_start(ap, _elt);
    for ( ; _elt; _elt = va_arg(ap, SEXP)) 
	SET_VECTOR_ELT(_vec, i++, _elt);
    va_end(ap);
    
    return i;
}

static int make_names(SEXP _names, const char *name, ...)
{
    int i = 0;
    va_list ap;
    va_start(ap, name);
    for ( ; name; name = va_arg(ap, const char *)) 
	SET_STRING_ELT(_names, i++, mkChar(name));
    va_end(ap);

    return i;
}

SEXP _ssmpdiff (SEXP _x, SEXP _y, SEXP _z)
{
    double *x = REAL(_x);
    double *y;
    int nx = length(_x);
    int nz = length(_z);

    int i;
    for (i = 0; i < nz; i++) {
	y = REAL(_y) + i*nx;
	REAL(_z)[i] = (x[0]-y[0])*(x[0]-y[0]) +
	    (x[1]+x[3]-y[1]-y[3])*(x[1]+x[3]-y[1]-y[3]) +
	    (x[2]+x[6]-y[2]-y[6])*(x[2]+x[6]-y[2]-y[6]) +
	    (x[4]-y[4])*(x[4]-y[4]) +
	    (x[5]+x[7]-y[5]-y[7])*(x[5]+x[7]-y[5]-y[7]) +
	    (x[8]-y[8])*(x[8]-y[8]);
    }
    return R_NilValue;
}

SEXP _countpairs (SEXP _x, SEXP _nr, SEXP _nc)
{
    int nr = INTEGER(_nr)[0];
    int nc = INTEGER(_nc)[0];
    int *x = INTEGER(_x);
    int nry = (nr*nr-nr)/2;
    int ncy = 9;

    int *ytab = Calloc(nry*ncy, int);

    int i, j, k, yy = 0;
    int cnt[9];

    for (i = 0; i < nr-1; i++)
	for (j  = i+1; j < nr; j++) {

	    for (k = 0; k < ncy; k++)
		cnt[k] = 0;

	    int xi, xj;
	    for (k = 0; k < nc; k++) {
		xi = x[i + nr*k];
		xj = x[j + nr*k];
		if (xi != NA_INTEGER && xj != NA_INTEGER) 
		    cnt[xi*3 + xj]++;
	    }

	    for (k = 0; k < ncy; k++)
		ytab[yy++] = cnt[k];
	}

    int stacksize = 0;
    SEXP _ytab;
    INT2SEXP(ytab, _ytab, nry*ncy);
    Free(ytab);

    UNPROTECT(stacksize);
    return _ytab;
}
}

