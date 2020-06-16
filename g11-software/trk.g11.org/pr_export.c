/*
   pr_export.c - interface functions for OFFLINE packages

   Created:     May 1, 1998
   Last update: Aug 1, 1999

   Serguei Boiarinov
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "prlib.h"

#ifdef ONLINE
#include "sdakeys.h"
#else

/* following have to be consistent with anapatt.inc */

#undef nsgmx
#undef nsgcmx
#undef nclmx
#undef ntrmx

#define nsgmx  60
#define nsgcmx 30
#define nclmx  100
#define ntrmx  120

#include <trk_run_control.h>
#endif


#ifndef ONLINE
void ecinit(int *jw, int opt1, int opt2, int opt3, int runnum) {}
int  eclib(int *iw, const float threshold[3], int *ntrk, PRTRACK *trk) {return(0);}
#endif


int
praxial_(char *ntest0, char *ntest1, char *ntest2)
{
  return(praxial(*ntest0,*ntest1,*ntest2));
}

int
prstereo_(char *ntest0, char *ntest1, char *ntest2)
{
  return(prstereo(*ntest0,*ntest1,*ntest2));
}

/* changed !!! */
void
prscan_(int *mtest1, int *mtest2, int *isec, short idtime[6][192][6], int *found)
{
  *found = prscan(*mtest1, *mtest2, *isec, idtime);
  return;
}

#ifdef SL5

void
praxialscan_(int *mtest1, int *mtest2, int *found, int *p)
{
  *found = praxialscan(*mtest1, *mtest2, p);
  return;
}

void
prstereoscan_(int *mtest1, int *mtest2, int *found, int *p)
{
  *found = prstereoscan(*mtest1, *mtest2, p);
  return;
}

#endif

int prupdatelink(int, int, float vect1[6], float, int, int ecdigi2[3], int nw[6][6], float *);
void
prupdatelink_(int *mtest1, int *mtest2, float vect1[6], float *pin, int *ntof, int ecdigi2[3], int nw[6][6], int *found, float *p)
{
  *found = prupdatelink(*mtest1, *mtest2, vect1, *pin, *ntof, ecdigi2, nw, p);
  return;
}


#ifdef ONLINE

void
prinit_()
{
  prinit(getenv("LBOS"),sdakeys_.lanal[1],sdakeys_.lanal[2],sdakeys_.lanal[5]);
  return;
}

#else

void
prinit_()
{
  int i, lanal1, lanal2, lanal5;
  char *s, tmp[500],  str[1000];

  /* get directory name */
  i = 0;
  s = trktcl_.spar_prlink_loc;
  while(s[i] != ' ' && s[i] != 0) i++;
  strncpy(str,s,i);
  str[i] = '\0';

  /* if directory name is 'CLAS_PARMS', get actual location */
  if(!strncmp(str,"CLAS_PARMS",strlen("CLAS_PARMS"))) strcpy(str,getenv("CLAS_PARMS"));

  /* get file name */
  i = 0;
  s = trktcl_.spar_prlink_name;
  while(s[i] != ' ' && s[i] != 0) i++;
  strncpy(tmp,s,i);
  tmp[i] = '\0';

  /* full path */
  strcat(str,"/");
  strcat(str,tmp);

  printf("prinit_(): dictionary file >%s<\n",str);

  printf("trktcl_.ipar_trk_make_prlink=%d\n",trktcl_.ipar_trk_make_prlink);
  printf("trktcl_.ipar_trk_minhits[0]=%d\n",trktcl_.ipar_trk_minhits[0]);
  printf("trktcl_.ipar_trk_minhits[1]=%d\n",trktcl_.ipar_trk_minhits[1]);
  lanal1 =0; /* lanal1 = trktcl_.ipar_trk_make_prlink; - RECSIS can not write dictionary */
  lanal2 = trktcl_.ipar_trk_minhits[0];
  lanal5 = trktcl_.ipar_trk_minhits[1];
  if(lanal2 < 3) lanal2 = 3;
  if(lanal5 < 3) lanal5 = 3;

  prinit(str,lanal1,lanal2,lanal5);
  return;
}

#endif

void
prlib_(int *jw)
{
  static int runnum = 0;
  /* EC parameters */
  const float threshold[3] = {0.0001,0.0001,0.0003};
  const int option[3] = {0,0,0};
  int ntrk, ind, rnum;
  static int opt=1;
  static PRTRACK track[NTRACK];

  if((ntrk=prlib(jw,track)) > 0)
  {
if(ntrk > 10) return;
    /* ec reconstruction */
    if((ind = etNlink(jw,"HEAD",0)) > 0)
    {
      rnum = jw[ind+1];
    }
    else
    {
      rnum = 9999;
    }
    if(rnum != runnum)
    {
      runnum = rnum;
      printf("eclib: set run number = %d\n",runnum);
      ecinit(jw, option[0], option[1], option[2], runnum);
    }
    eclib(jw, threshold, &ntrk, track);
/*printf("ntrk=%d\n",ntrk);*/
    /*if(l3lib(jw, ntrk, track, opt))*/
    {
      prbos(jw, &ntrk, track); /* fill output BOS bank */
    }
  }

  return;
}


void
prwrite_()
{
  prwrite("prlink.bos");
  return;
}


void
prsegmlist_(int *iw3, short idtime[192][6], int *ifound, int *ngr, int *min_hit_seg)
{
  prsegmlist(*iw3, idtime, ifound, *ngr, *min_hit_seg);
}

void
prupdatesegm_(int nw[6])
{
 prupdatesegm(nw);
}







void
proutput_(int *iw, int *sec, int segm[6][nsgmx][12], int nsegm[6], int clust[6][nclmx][nsgcmx],
          int nsegmc[6][nclmx], int nclust[6],
          int *ntr_link, int itr_level[ntrmx], int itr_sect[ntrmx],
          int lnk_segm[ntrmx][6], int lnk_clust[ntrmx][6], float lnk_vect[ntrmx][nlnkvect],
          float lnk_ec[ntrmx][nlnkec])
{
  PRTRACK *trkptr, *trkptr1, *track;
  PRCLUSTER *clustptr;
  PRSEGMENT *sgmptr;
  int i, j, itr, itr1, is, iv, icl, isg, ncoin, trkcount, io;
  int *ptri, ind, nhits, tmp, ntrack;
  float *ptrf;

  for(is=0; is<6; is++)
  {
    nsegm[is] = 0;
    nclust[is] = 0;
    for(i=0; i<nclmx; i++) nsegmc[is][i] = 0;
  }


  if((ind = etNlink(iw,"PATH",0)) <= 0)
  {
    return;
  }
  ntrack = (etNdata(iw,ind)*sizeof(int))/sizeof(PRTRACK);
  track = (PRTRACK *)&iw[ind];


/* check for close tracks

  for(itr=0; itr<ntrack; itr++)
  {
    trkptr = &track[itr];
    if(trkptr->sector != *sec) continue;
    for(itr1=itr+1; itr1<ntrack; itr1++)
    {
      trkptr1 = &track[itr1];
      ncoin = 0;
      for(is=0; is<6; is++)
      {
        if(trkptr->cluster[is].segment[0].iw[2] == trkptr1->cluster[is].segment[0].iw[2]) ncoin++;
      }
      if(ncoin > 4)
      {
        printf("EEEEEEEE1 %d -> %d %d %d %d %d %d\n",itr,
trkptr->cluster[0].segment[0].iw[2],trkptr->cluster[1].segment[0].iw[2],trkptr->cluster[2].segment[0].iw[2],
trkptr->cluster[3].segment[0].iw[2],trkptr->cluster[4].segment[0].iw[2],trkptr->cluster[5].segment[0].iw[2]);
        printf("EEEEEEEE2 %d -> %d %d %d %d %d %d\n",itr1,
trkptr1->cluster[0].segment[0].iw[2],trkptr1->cluster[1].segment[0].iw[2],trkptr1->cluster[2].segment[0].iw[2],
trkptr1->cluster[3].segment[0].iw[2],trkptr1->cluster[4].segment[0].iw[2],trkptr1->cluster[5].segment[0].iw[2]);
      }
    }
  }
*/





  /* copy results to SDA-RECSIS data structures */

  nhits = 0;
  trkcount = 0;
  for(itr=0; itr<ntrack; itr++)
  {
    trkptr = &track[itr];
    if(trkptr->sector != *sec) continue;
    for(is=0; is<6; is++)
    {
      clustptr = &trkptr->cluster[is];
      /*printf("prlinksegm_: clustptr->nsegment=%d\n",clustptr->nsegment);*/
      for(isg=0; isg<clustptr->nsegment; isg++)
      {
        sgmptr = &clustptr->segment[isg];
        for(i=0; i<6; i++)
        {
          segm[is][nsegm[is]][2*i]   = sgmptr->iw[i];
          segm[is][nsegm[is]][2*i+1] = sgmptr->tdc[i];
          if(sgmptr->iw[i] > 0) nhits ++;
        }
	/*printf("prlinksegm_: nsegm[%1d]=%d, iw=%d %d %d %d %d %d\n",is,nsegm[is],
                       sgmptr->iw[0],sgmptr->iw[1],sgmptr->iw[2],sgmptr->iw[3],sgmptr->iw[4],sgmptr->iw[5]);*/
        clust[is][nclust[is]][nsegmc[is][nclust[is]]] = nsegm[is] + 1;

        lnk_clust[itr][is] = nclust[is] + 1;
        lnk_segm[itr][is]  = clust[is][nclust[is]][nsegmc[is][nclust[is]]];

        nsegm[is] ++;
        if(nsegm[is] >= nsgmx) {/*printf("proutput: ERROR - exceed nsgmx\n");*/ goto exit;}
        nsegmc[is][nclust[is]] ++;
        if(nsegmc[is][nclust[is]] >= nsgcmx) {printf("proutput: ERROR - exceed nsgcmx\n"); goto exit;}
      }
      nclust[is] ++;
      if(nclust[is] >= nclmx) {/*printf("proutput: ERROR - exceed nclmx\n");*/ goto exit;}
    }
    itr_level[itr] = 1;
    itr_sect[itr]  = trkptr->sector;

    for(iv=0; iv<6; iv++) lnk_vect[itr][iv] = trkptr->vect[iv];
    /* correction !!! */
    lnk_vect[itr][0] = lnk_vect[itr][0] - 3.2;
    lnk_vect[itr][1] = lnk_vect[itr][1] + 0.8;

    lnk_vect[itr][6] = trkptr->p;

    lnk_vect[itr][7] = (float)trkptr->ntof;
    lnk_vect[itr][8] = (float)trkptr->nu / 2.;
    lnk_vect[itr][9] = (float)trkptr->nv / 2.;
    lnk_vect[itr][10] = (float)trkptr->nw / 2.;
    lnk_vect[itr][11] = (float)trkptr->charge;

    for(j=0; j<nlnkec; j++) lnk_ec[itr][j]  = 0.0;
    if(trkptr->ec[2].e > 0.0001)
    {
      for(io=0; io<3; io++)
      {
        lnk_ec[itr][io*4  ] = trkptr->ec[io].u;
        lnk_ec[itr][io*4+1] = trkptr->ec[io].v;
        lnk_ec[itr][io*4+2] = trkptr->ec[io].w;
        lnk_ec[itr][io*4+3] = trkptr->ec[io].e;
      }
    }

    /*
    printf("vect[%d]=%6d %6d %6d %6d %6d %6d %6d - %6d - %6d %6d %6d - %6d\n",
                             itr,trkptr->vect[0],trkptr->vect[1],trkptr->vect[2],
                             trkptr->vect[3],trkptr->vect[4],trkptr->vect[5],trkptr->p,
                             trkptr->ntof,trkptr->nu/2,trkptr->nv/2,trkptr->nw/2,trkptr->charge);
    */
    trkcount ++;
  }


  (*ntr_link) += trkcount;
  goto dc1;

exit:

  (*ntr_link) = itr;

dc1:
  /* make DC1 bank */

  /*printf("proutput: nhits=%d sec=%d\n",nhits,*sec);*/
  if(nhits)
  {
    etnformat_(iw,"DC1 ","I,F",4,3);
    tmp=2;
    if((ind = etncreate_(iw,"DC1 ",sec,&tmp,&nhits,4)) > 0)
    {
      /*printf(">>> sec=%d nhits=%d(bytes=%d) ind=%d\n",*sec,nhits,nhits*8,ind);*/
      ptri = (int *)&iw[ind];
      ptrf = (float *)&iw[ind+1];
      for(itr=0; itr<(*ntr_link); itr++)
      {
        trkptr = &track[itr];
        if(trkptr->sector != *sec) continue;
        for(is=0; is<6; is++)
        {
          clustptr = &trkptr->cluster[is];
          for(isg=0; isg<clustptr->nsegment; isg++)
          {
            sgmptr = &clustptr->segment[isg];
            for(i=0; i<6; i++)
            {
              if(sgmptr->iw[i] > 0)
              {
		
                *ptri = sgmptr->iw[i] + 256*(6*is + i+1);
                *ptrf = sgmptr->tdc[i];
		
                ptri+=2;
                ptrf+=2;
              }
            }
          }
        }
      }
      /*
      ptri = (int *)&iw[ind];
      ptrf = (float *)&iw[ind+1];
      for(i=0; i<nhits; i++)
      {
        printf(">>> %6d(iw=%3d  l=%3d %f\n",*ptri,(*ptri)&0xff,((*ptri)&0xff00)>>8,*ptrf);
        ptri+=2; ptrf+=2;
      }
      */
    }
  }

  return;
}









