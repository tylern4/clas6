#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <clas_cern.h>
#define MAXHISTO 100000




void hlimit(int size)
{
  hlimit_(&size);
}

float hstati(int id, int icase, char *choice, int num){
  return(hstati_(&id, &icase, choice, &num, strlen(choice)));
}

float hx(int id, float x){
  return(hx_(&id, &x));
}

void hxi(int id, float x, int *bin){
  hxi_(&id, &x, bin);
}

float hsum(int id){
  return(hsum_(&id));
}

void hrput(int id, char *file, char *opt)
{
  hrput_(&id,file,opt,strlen(file),strlen(opt));
}


void hf1(int id, float data, float weight)
{
  hf1_(&id,&data,&weight);
}

void hbarx(int id){
  hbarx_(&id);
} 

void hf1e(int id, float data, float weight, float error)
{
  hf1e_(&id,&data,&weight, &error);
}

void hf2(int id,float data1,float data2,float weight)
{
  hf2_(&id,&data1,&data2,&weight);
}

void hdelet(int id)
{
	hdelet_(&id);
}

void hrin( int id, int icycle , int iofset )
{
        hrin_( &id, &icycle, &iofset);
}

void hfn(int id, float data[])
{
  hfn_(&id,&data[0]); 
}

void hfithn(int id, char *chfun, char *chopt, int np, float par[],
        float step[], float pmin[], float pmax[], float sigpar[], float *chi2)
{
  hfithn_(&id, chfun, chopt, &np, par, step, pmin, pmax, sigpar, chi2,
	  strlen(chfun), strlen(chopt));

}

void hunpak(int histo,float contents[],char choice[], int num)
{
  hunpak_(&histo,contents,choice,&num,strlen(choice));
}

void hidopt( int id, char *chopt)
{
        hidopt_( &id, chopt ,  strlen( chopt));
}

void hpak(int histo,float contents[])
{
  hpak_(&histo,contents);
}

void hpakad(int histo,float contents[])
{
  hpakad_(&histo,contents);
}

void hpake(int id,float errors[]){
  hpake_(&id,errors);
}

void hunpke(int id,float contents[],char choice[], int num){
  hunpke_(&id,contents,choice,&num,strlen(choice));
}

void hbslix(int id,int slices,float weight){
  hbslix_(&id,&slices,&weight);
}

void hbsliy(int id,int slices,float weight){
  hbsliy_(&id,&slices,&weight);
}

void hcopy(int id1,int id2,char title[]){
  hcopy_(&id1,&id2,title,strlen(title));
}

void hrget( int id, char *chfile, char *chopt)
{
  hrget_( &id, chfile, chopt, strlen(chfile), strlen(chopt));
}

void hldir(char dir[],char flag[])
{
  hldir_(dir,flag,strlen(dir),strlen(flag));
}

void hmdir(char dir[],char flag[])
{
  hmdir_(dir,flag,strlen(dir),strlen(flag));
}

void hcdir(char dir[],char flag[])
{
  hcdir_(dir,flag,strlen(dir),strlen(flag));
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

void hropen(int lun, char *name, char*filename, char*status, int stor,
        int istat)
{
        hropen_(&lun, name, filename, status, &stor, &istat, strlen(name),
                strlen(filename), strlen(status));
        /*warning the value of istat will be lost*/
        return;
}

void hrout(int num, int icycle, char*opt)
{
        hrout_(&num, &icycle, opt, strlen(opt));
        return;
}

void hrend(char*filename)
{
        hrend_(filename, strlen(filename));
        return;
}


void hreset(int no, char* opt)
{
        hreset_(&no, opt, strlen(opt));
        return;
}

void hbook2(int no, char*name, int xbins, float xmin, float xmax, int ybins,
                 float ymin, float ymax, float weight)
{
  hbook2_(&no, name, &xbins, &xmin, &xmax, &ybins,&ymin, &ymax, &weight, strlen(name));
}


void hbook1(int no, char*name, int nbins, float min, float max, float v)
{
  hbook1_(&no, name, &nbins, &min, &max, &v, strlen(name));
}

/* assume CHTAGS is space padded such that each tag is strlen(CHTAGS)/NVAR characters */
void hbookn(int id, char *CHTITL,int NVAR, char *CHRZPA,int NWBUFF,char*CHTAGS)
{
	if(((strlen(CHTAGS)/NVAR)*NVAR)!=strlen(CHTAGS)){
		fprintf(stderr,"%s:%d Bad string length passed to hbookn()\n",__FILE__,__LINE__);
	}
	hbookn_(&id,CHTITL,&NVAR,CHRZPA,&NWBUFF,CHTAGS,strlen(CHTITL),strlen(CHRZPA),strlen(CHTAGS)/NVAR);
}

void hfill(int no, float xvalue, float yvalue, float weight)
{
        hfill_(&no, &xvalue, &yvalue, &weight);
}

/* Wrapper for hbook histogram operations subroutine */
void hopera(int id1,char *oper,int id2, int id3,float scale1,float scale2){
  
  hopera_(&id1,oper,&id2,&id3,&scale1,&scale2,strlen(oper));

}

//*** CWN Wrappers *******************
void hbnt(int id,char *name,char *opt)
{
  int kid = id;
  hbnt_(&kid,name,opt,strlen(name),strlen(opt));
}

void hbname(int id,char *name,void *variable,char *form)
{
  int kid = id;
  hbname_(&kid,name,variable,form,strlen(name),strlen(form));
}

//**** Wrappers to fill a CW Ntuple ******

void hfnt(int id)
{
  int kid = id;
   hfnt_(&kid);
}

void hfntb(int id,char *CHBLOK)
{
  int kid = id;
   hfntb_(&kid,CHBLOK);
}


//******** Wrappers to HRNDM routines ************

float hrndm1(int id)
{
  int x = id;
  float ret = hrndm1_(&x);
  return(ret);
}

float hrndm2(int id,float *x, float *y)
{
  return(hrndm2_(&id,x,y));
}



