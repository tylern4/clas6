/* 
 * prototypes for c_cern package
 *
 * clas c interface to hbook and hplot
 *
 *  jm, ejw, 8-jan-1998
 *
 */

/* macros for choice variable*/
#define HSTATI_MEAN 1
#define HSTATI_STDEV 2
#define HSTATI_NEQUIV 3

#ifdef __cplusplus
extern "C" {
#endif

/* hbook: http://wwwasdoc.web.cern.ch/wwwasdoc/hbook/H1HBOOK-Tabular-Overview.html
          http://wwwasdoc.web.cern.ch/wwwasdoc/hbook_html3/hboomain.html */

void hbarx(int id);
void hbarx_(const int* id);

void hbfun1(int id, const char *name, int nx, float xmin, float xmax, void* fun);
void hbfun1_(const int* id, const char* name, const int* nx, const float* xmin,
	     const float* xmax, void* fun, const int);

void hbook1(int id, const char* name, int nbins, float min, float max, float vmx);
void hbook1_(const int* id, const char* name, const int* nbins,
	     const float* min, const float* max, const float* vmx, const int);

void hbook2(int id, const char* name, int xbins, float xmin, float xmax,
	    int ybins, float ymin, float ymax, float weight);
void hbook2_(const int* id, const char* name, const int* xbins,
	     const float* xmin, const float* xmax, const int* ybins,
	     const float* ymin, const float* ymax, const float* vmx, const int);

void hbprof(int id, const char* name, int ncx, float xlow, float xup,
	    float ymin, float ymax, const char* opt);
void hbprof_(const int* id, const char* name, const int* ncx, const float* xlow,
	     const float* xup, const float* ymin, const float* ymax,
	     const char* opt, const int, const int);

void hbprox(int id, float vmx);
void hbprox_(const int* id, const float* vmx);

void hcdir(char path[], const char opt[]);
void hcdir_(char* path, const char* opt, const int, const int);

void hcopy(int id1, int id2, const char* name);
void hcopy_(const int* id1, const int* id2, const char* name, const int);

void hdelet(int id);
void hdelet_(const int* id);

/* hbook function returns logical */
int hexist(int id);
int hexist_(const int *id);

void hf1(int id, float x, float weight);
void hf1_(const int* id, const float* x, const float* weight);

void hf1e(int id, float x, float weight, float error);
void hf1e_(const int* id, const float* x, const float* weight,
	   const float* error);

void hf2(int id, float x, float y, float weight);
void hf2_(const int* id, const float* x, const float* y,
	  const float* weight);

void hfill(int id, float x, float y, float weight);
void hfill_(const int* id, const float* x, const float* y, const float* weight);

void hfithn(int id, char *fun, char *opt, int np, float par[], float step[],
	    float pmin[], float pmax[], float sigpar[], float *chi2);
void hfithn_(const int* id, const char* fun, const char* opt,
	     const int* np, float* par, const float* step,
	     const float* pmin, const float* pmax, float* sigpar,
	     float* chi2, const int, const int);

void hfn(int id, float data[]);
void hfn_(const int* id, const float* data);

void hgive(int id, char *name, int* nx, float* xmin, float* xmax,
	   int* ny, float* ymin, float* ymax, int* nwt, int* loc);
void hgive_(const int* id, char* name, int* nx, float* xmin, float* xmax,
	    int* ny, float* ymin, float* ymax, int* nwt, int* loc, const int);

void hgiven(int id, char *name, int *NVAR, char *tag, float *RLOW, float *RHIGH);
void hgiven_(const int *id, char *name, int *NVAR, char *tag, float *RLOW,
	     float *RHIGH, const int, const int);

void hgn(int id, int* IDN, int IDNEVT, float* X, int* IERR);
void hgn_(const int* id, int* IDN, const int* IDNEVT, float* X, int* IERR);

void hgnf(int id, int IDEVENT, float* X, int* IERROR);
void hgnf_(const int* id, const int* IDEVENT, const float* x,
	   const int* IERROR);

void hgnpar(int id, const char *rout);
void hgnpar_(const int* id, const char* rout, const int);

float hi(int id, int I);
float hi_(const int* id, int* I);

void hix(int id, int I, float* X);
void hix_(const int* id, const int* I, float* X);

void hidall(int *IDVECT, int *N);
void hidall_(int* IDVECT, int* N);

void hidopt(int id, const char *opt);
void hidopt_(const int* id, const char* opt, const int);

void hldir(const char* path, const char* opt);
void hldir_(const char* path, const char* opt, const int, const int);

void hlimit(int nwpaw);
void hlimit_(const int* nwpaw);

float hmax(int id);
float hmax_(const int *id);

void hmdir(const char* path, const char* opt);
void hmdir_(const char* path, const char* opt, const int, const int);

void hnoent(int id, int* noent);
void hnoent_(const int* id, int* noent);

void hopera(int id1, const char *oper, int id2, int id3, float scale1, float scale2);
void hopera_(const int* id1, const char* oper, const int* id2, const int* id3,
	     const float* scale1, const float* scale2, const int);

void hpak(int id, float contents[]);
void hpak_(const int* id, const float* contents);

void hrend(const char* filename);
void hrend_(const char* filename, const int);

void hreset(int id, const char* opt);
void hreset_(const int* id, const char* name, const int);

void hrget(int id, char *file, char *opt);
void hrget_(const int* id, const char *file, const char* opt,
	    const int, const int);

void hrin(int id, int icycle, int iofset);
void hrin_(const int* id, const int* icycle, const int* iofset);

void hropen(int lun, const char *top, const char* file, const char* opt,
	    int stor, int istat);
void hropen_(const int* lun, const char* top, const char* file, const char* opt,
	     int* stor, int* istat, const int, const int, const int);

void hrout(int num, int icycle, const char* opt);
void hrout_(const int* id, int* icycle, const char* opt, const int);

void hrput(int id, char *file, char *opt);
void hrput_(const int* id, const char* file, const char* opt,
	    const int, const int);

float hstati(const int id, int icase, char* choice, int num);
float hstati_(const int* id, const int* icase, char* choice,
	      const int* num, const int);

float hsum(int id);
float hsum_(const int* id);

void hunpak(int id, float* contents, const char* choice, int num);
void hunpak_(const int* id, float* contents, const char* choice,
	     const int* num, const int);

float hx(int id, float x);
float hx_(const int* id, const float* x);

void hxi(int id, float x, int *bin);
void hxi_(const int* id, const float* x, int* bin);


/* hplot: http://wwwasdoc.web.cern.ch/wwwasdoc/higz/H2OverviewofHPLOTcalls.html */

void hplcap(int unit);
void hplcap_(const int* unit);
/*
void hplcon(int id, int x, int y);
void hplcon_(const int id, const int* x, const int* y);
*/
void hplego(int id, float theta, float phi);
void hplego_(const int* id, const float* theta, const float* phi);

void hplint(int id);
void hplint_(const int* id);

void hplopt(const char *opt, int N);
void hplopt_(const char* opt, const int* n, const int);

void hplot(int id, const char* opt, const char* chcase, int num);
void hplot_(const int* id, const char* opt, const char* chcase,
	    const int* num, const int, const int);

void hplset(const char *opt, float var);
void hplset_(const char* opt, const float* var, const int);

void hplzom(int id, const char* opt, int min, int max);
void hplzom_(const int* id, const char* opt, const int* min,
	     const int* max, const int);
/*
void hplzon(int x, int y, int num, char *opt);
void hplzon_(const int* x, const int* y, const int* num, const char* opt, const int);
*/

/* higz: http://wwwasdoc.web.cern.ch/wwwasdoc/higz/H2Workstationtypes.html */

void ipl(int N, float* X, float* Y);
void ipl_(const int* N, const float* X, const float* Y);

void ischh(float CHH);
void ischh_(const float* CHH);

void iselnt(int NT);
void iselnt_(const int* NT);

void isln(int type);
void isln_(const int* type);

void islwsc(int width);
void islwsc_(const int* width);

void isplci(int i);
void isplci_(const int* i);

void itx(float X, float Y, const char *CHARS);
void itx_(const float* X, const float* Y, const char* CHARS, const int);

void iuwk(int num1, int num2);
void iuwk_(const int* num1, const int* num2);

void ixupdwi(int N);
void ixupdwi_(const int* N);


/* ? */

void hfilpr(int id);
void hfilpr_(const int* id);

void mninit(int IRD, int IWR, int ISAV);
void mninit_(const int* IRD, const int* IWR, const int* ISAV);

#ifdef __cplusplus
}
#endif
