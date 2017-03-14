#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include "c_cern.h"

#define MAXHISTO 10000

/*===========================================================*/
/* While the routines here are not multi-threaded, they
/* can be called by several threads simultaneously. Hence,
/* we use a mutex to ensure only one routine is accessing
/* the pawc_ comman at a time.
/*===========================================================*/
pthread_mutex_t CERN_MUTEX = PTHREAD_MUTEX_INITIALIZER;

#define CERNSERIALIZE(A) {\
   pthread_mutex_lock(&CERN_MUTEX);\
   A;\
   pthread_mutex_unlock(&CERN_MUTEX);}

#define CERNSERIALIZEF(A) {\
   float F;\
   pthread_mutex_lock(&CERN_MUTEX);\
   F=A;\
   pthread_mutex_unlock(&CERN_MUTEX);\
   return F;}

#define CERNSERIALIZEI(A) {\
   int I;\
   pthread_mutex_lock(&CERN_MUTEX);\
   I=A;\
   pthread_mutex_unlock(&CERN_MUTEX);\
   return I;}


void hlimit(int size)
{
  CERNSERIALIZE(hlimit_(&size));
}

float hstati(int id, int icase, char *choice, int num){
   CERNSERIALIZEF(hstati_(&id, &icase, choice, &num, strlen(choice)));
}

float hx(int id, float x){
   CERNSERIALIZEF(hx_(&id, &x));
}

void hxi(int id, float x, int *bin){
  CERNSERIALIZE(hxi_(&id, &x, bin));
}

float hsum(int id){
   CERNSERIALIZEF(hsum_(&id));
}

void hrput(int id, char *file, char *opt)
{
  CERNSERIALIZE(hrput_(&id,file,opt,strlen(file),strlen(opt)));
}

void hf1(int id, float data, float weight)
{
  CERNSERIALIZE(hf1_(&id,&data,&weight));
}

void hbarx(int id){
  CERNSERIALIZE(hbarx_(&id));
}

void hf1e(int id, float data, float weight, float error)
{
  CERNSERIALIZE(hf1e_(&id,&data,&weight, &error));
}

void hf2(int id,float data1,float data2,float weight)
{
  CERNSERIALIZE(hf2_(&id,&data1,&data2,&weight));
}

void hdelet(int id)
{
	CERNSERIALIZE(hdelet_(&id));
}

void hrin( int id, int icycle , int iofset )
{
   CERNSERIALIZE(hrin_( &id, &icycle, &iofset));
}

void hfn(int id, float data[])
{
  CERNSERIALIZE(hfn_(&id,&data[0]));
}

void hfithn(int id, char *chfun, char *chopt, int np, float par[],
        float step[], float pmin[], float pmax[], float sigpar[], float *chi2)
{
  CERNSERIALIZE(hfithn_(&id, chfun, chopt, &np, par, step, pmin, pmax
     , sigpar, chi2,strlen(chfun), strlen(chopt)));

}

void hunpak(int histo, float contents[], const char choice[], int num)
{
  CERNSERIALIZE(hunpak_(&histo,contents,choice,&num,strlen(choice)));
}

void hix(int HID,int I,float *X)
{
	CERNSERIALIZE(hix_(&HID,&I,X));
}

float hi(int HID,int I)
{
	CERNSERIALIZEF(hi_(&HID,&I));
}

void hidopt(int id, const char *chopt)
{
   CERNSERIALIZE(hidopt_( &id, chopt ,  strlen( chopt)));
}

void hpak(int histo,float contents[])
{
  CERNSERIALIZE(hpak_(&histo,contents));
}

void hrget( int id, char *chfile, char *chopt)
{
  CERNSERIALIZE(hrget_( &id, chfile, chopt, strlen(chfile), strlen(chopt)));
}

void hldir(const char dir[], const char flag[])
{
  CERNSERIALIZE(hldir_(dir,flag,strlen(dir),strlen(flag)));
}

void hmdir(const char dir[], const char flag[])
{
  CERNSERIALIZE(hmdir_(dir,flag,strlen(dir),strlen(flag)));
}

void hcdir(char dir[],const char flag[])
{
  CERNSERIALIZE(hcdir_(dir,flag,strlen(dir),strlen(flag)));
}

/*
void hplzon(int x, int y, int num, char *opt)
{
  hplzon_(&x, &y, &num, opt, strlen(opt));
}

void hplcon(int histonum, int x, int y)
{
  hplcon_(&histonum, &x, &y);
}
*/

void hropen(int lun, const char *name, const char *filename,
	    const char* status, int stor, int istat)
{
   CERNSERIALIZE(hropen_(&lun, name, filename, status, &stor, &istat
      , strlen(name),strlen(filename), strlen(status)));
   /*warning the value of istat will be lost*/
}

void hrout(int num, int icycle, const char* opt)
{
   CERNSERIALIZE(hrout_(&num, &icycle, opt, strlen(opt)));
}

void hrend(const char* filename)
{
   CERNSERIALIZE(hrend_(filename, strlen(filename)));
}

void hreset(int no, const char* opt)
{
   CERNSERIALIZE(hreset_(&no, opt, strlen(opt)));
}

void hbook2(int no, const char *name, int xbins, float xmin, float xmax,
	    int ybins, float ymin, float ymax, float weight)
{
  CERNSERIALIZE(hbook2_(&no, name, &xbins, &xmin, &xmax, &ybins,&ymin
     , &ymax, &weight, strlen(name)));
}

void hbook1(int no, const char *name, int nbins, float min, float max, float v)
{
  CERNSERIALIZE(hbook1_(&no, name, &nbins, &min, &max, &v, strlen(name)));
}

void hfill(int no, float xvalue, float yvalue, float weight)
{
   CERNSERIALIZE(hfill_(&no, &xvalue, &yvalue, &weight));
}

/* Wrapper for hbook histogram operations subroutine */
void hopera(int id1, const char *oper, int id2, int id3,
	    float scale1, float scale2)
{  
  CERNSERIALIZE(hopera_(&id1,oper,&id2,&id3,&scale1,&scale2,strlen(oper)));
}

int hexist(int HID)
{
   CERNSERIALIZEI(hexist_(&HID));
}

void hplint(int no)
{
   CERNSERIALIZE(hplint_(&no));
}

void iuwk(int num1, int num2)
{
   CERNSERIALIZE(iuwk_(&num1, &num2));
}

void hplego(int no, float theta, float phi)
{
   CERNSERIALIZE(hplego_(&no, &theta, &phi));
}

void hplcap(int unit)
{
  CERNSERIALIZE(hplcap_(&unit));
}

void hplot(int no, const char* chopt, const char* chcase, int num)
{
   CERNSERIALIZE(hplot_(&no,chopt,chcase,&num,strlen(chopt),strlen(chcase)));
}

void hplzom(int hid, const char* chopt, int min, int max)
{
   CERNSERIALIZE(hplzom_(&hid,chopt,&min,&max,strlen(chopt)));
}
/*
void hplzon(int nx, int ny, int firstplotted, char *option){
  CERNSERIALIZE(hplzon_(&nx, &ny, &firstplotted, option, strlen(option)));
}
*/
void hgn(int NID,int *IDN,int IDNEVT,float *X,int *IERR)
{
   CERNSERIALIZE(hgn_(&NID,IDN,&IDNEVT,X,IERR));
}

void hgiven(int NID,char *CHTITL,int *NVAR,char *CHTAG,float *RLOW,float *RHIGH)
{
   CERNSERIALIZE(hgiven_(&NID,CHTITL,NVAR,CHTAG,RLOW,RHIGH,32L,16L));
}

void hidall(int *IDVECT, int *N)
{
   CERNSERIALIZE(hidall_(IDVECT,N));
}

void hnoent(int ID,int *NOENT)
{
   CERNSERIALIZE(hnoent_(&ID,NOENT));
}

void hgnpar(int ID, const char *CHROUT)
{
   CERNSERIALIZE(hgnpar_(&ID,CHROUT,strlen(CHROUT)));
}

void hgnf(int ID, int IDEVENT, float* X, int* IERROR)
{
   CERNSERIALIZE(hgnf_(&ID,&IDEVENT,X,IERROR));
}

void hbprof(int ID, const char* CHTITL, int NCX, float XLOW, float XUP,
	    float YMIN, float YMAX, const char *CHOPT)
{
   CERNSERIALIZE(hbprof_(&ID,CHTITL,&NCX,&XLOW,&XUP,&YMIN,&YMAX,CHOPT
      ,strlen(CHTITL),strlen(CHOPT)));
}

void ixupdwi(int N)
{
   CERNSERIALIZE(ixupdwi_(&N));
}

void hcopy(int ID1, int ID2, const char* CHTITL)
{
   CERNSERIALIZE(hcopy_(&ID1,&ID2,CHTITL,strlen(CHTITL)));
}

void hplopt(const char* CHOPT, int N)
{
   CERNSERIALIZE(hplopt_(CHOPT,&N,4L));
}

void hplset(const char* CHOPT, float VAR)
{
   CERNSERIALIZE(hplset_(CHOPT,&VAR,4L));
}

void ipl(int N, float* Y, float* X)
{
   CERNSERIALIZE(ipl_(&N,Y,X));
}

void isplci(int i)
{
   CERNSERIALIZE(isplci_(&i));
}

void isln(int type)
{
   CERNSERIALIZE(isln_(&type));
}

void islwsc(int width)
{
   CERNSERIALIZE(islwsc_(&width));
}

float hmax(int id)
{
   CERNSERIALIZEF(hmax_(&id));
}

void iselnt(int NT)
{
   CERNSERIALIZE(iselnt_(&NT));
}

void itx(float X, float Y, const char* CHARS)
{
   CERNSERIALIZE(itx_(&X,&Y,CHARS,strlen(CHARS)));
}

void ischh(float CHH)
{
   CERNSERIALIZE(ischh_(&CHH));
}

void hgive(int ID,char *CHTITL,int *NX,float *XMI,float *XMA
   			,int *NY,float *YMI,float *YMA,int *NWT,int *LOC)
{
   CERNSERIALIZE(hgive_(&ID,CHTITL,NX,XMI,XMA,NY,YMI,YMA,NWT,LOC,80L));
}

void hbfun1(int FID, const char *CHTIT, int NX, float XMI, float XMA, void *fun)
{
   CERNSERIALIZE(hbfun1_(&FID,CHTIT,&NX,&XMI,&XMA,fun,strlen(CHTIT)));
}

void mninit(int IRD, int IWR,int ISAV)
{
	CERNSERIALIZE(mninit_(&IRD,&IWR,&ISAV));
}

void hbprox(int HID, float VX)
{
	CERNSERIALIZE(hbprox_(&HID,&VX));
}

void hfilpr(int HID)
{
	CERNSERIALIZE(hfilpr_(&HID));
}

