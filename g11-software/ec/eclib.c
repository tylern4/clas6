
/************************************************

  eclib.c - forward calorimeter reconstruction

  BUG report: boiarino@cebaf.gov

*************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "eclib.h"

#define MAX(a,b)    (a > b ? a : b)
#define MIN(a,b)    (a < b ? a : b)
#define ABS(x)      ((x) < 0 ? -(x) : (x))

/* for FPACK format the length of bank are located in different place */
#ifdef FPACK
#define IDATA 2
#else
#define IDATA 1
#endif

/*
FILE *outfile;
*/

/* variables global for present file */

static int runnum = 3166; /* temporarily */
static float values[NSTRIP*NSECTOR];
static char map[100], *dir;
static float strip_threshold;

static int *iw; /* pointer to BOS array */
static ECPar geometry[6][2];

/* ecstrips.c - loading strip info for particular sector

  input:  sector - sector number

  output: strip[][].energy - strip energy (MeV)
          strip[][].time   - strip time (ns)
*/


int ecstrips_(int *jw, float *threshold, int *def_adc, int *def_tdc, int *sector, ECStrip strip[NLAYER][NSTRIP])
{
  return(ecstrips(jw, *threshold, *def_adc, *def_tdc, (*sector)-1, strip));
}

int ecstrips(int *jw, float threshold, int def_adc, int def_tdc, int sec, ECStrip strip[NLAYER][NSTRIP])
{
  static float e0 [NSECTOR][NLAYER][NSTRIP]; /* pedestals (channels) */
  static float ech[NSECTOR][NLAYER][NSTRIP];
  static float t0 [NSECTOR][NLAYER][NSTRIP]; /* delays (nsec) */
  static float tch[NSECTOR][NLAYER][NSTRIP];
  static int first=1;

  ECDataPtr ec;
  int i, j, k, ind, nhits, str, layer, firsttime, error, ii;
  float energy;
  char *str1[6] = {"inner","inner","inner","outer","outer","outer"};
  char *str2[6] = {"u","v","w","u","v","w"};
  char *map_e0 = "EC_PEDESTALS", *map_ech = "EC_GAIN",*map_t0 = "EC_T0", *map_tch = "EC_TCH";


  if(first)
  {
/*
outfile = fopen("test.txt","w");
*/
    printf("eclib: def_adc=%d     def_tdc=%d\n",def_adc,def_tdc);
    for(i=0; i<NSECTOR; i++)
    {
      for(j=0; j<NLAYER; j++)
      {
        for(k=0; k<NSTRIP; k++)
        {
          e0 [i][j][k] = 0.0;
          ech[i][j][k] = 0.0001;
          t0 [i][j][k] = 0.0;
          tch[i][j][k] = 0.05;
        }
      }
    }

    dir = getenv("CLAS_PARMS");

    if(!def_adc)
    {
      for(j=0; j<NLAYER; j++)
      {
        sprintf(map,"%s/Maps/%s.map",dir,map_e0);
        printf("eclib: reading file %s\n",map);
        map_get_float(map,str1[j],str2[j],NSTRIP*NSECTOR,values,runnum,&firsttime);
        for(i=0; i<NSECTOR; i++)
        {
          for(k=0; k<NSTRIP; k++)
          {
            if(values[NSTRIP*i+k] != 0) e0[i][j][k] = values[NSTRIP*i+k];
/*
fprintf(outfile,"%d %d %d ===> %f\n",i,j,k,e0[i][j][k]);
*/
          }
        }
        sprintf(map,"%s/Maps/%s.map",dir,map_ech);
        printf("eclib: reading file %s\n",map);
        map_get_float(map,str1[j],str2[j],NSTRIP*NSECTOR,values,runnum,&firsttime);
        for(i=0; i<NSECTOR; i++)
        {
          for(k=0; k<NSTRIP; k++)
	  {
            if(values[NSTRIP*i+k] != 0) ech[i][j][k] = values[NSTRIP*i+k];
          }
        }
      }
    }

    if(!def_tdc)
    {
      for(j=0; j<NLAYER; j++)
      {
        sprintf(map,"%s/Maps/%s.map",dir,map_t0);
        printf("eclib: reading file %s\n",map);
         map_get_float(map,str1[j],str2[j],NSTRIP*NSECTOR,values,runnum,&firsttime);
        for(i=0; i<NSECTOR; i++)
        {
          for(k=0; k<NSTRIP; k++)
          {
            if(values[NSTRIP*i+k] != 0) t0[i][j][k] = values[NSTRIP*i+k];
          }
        }
        sprintf(map,"%s/Maps/%s.map",dir,map_tch);
        printf("eclib: reading file %s\n",map);
        map_get_float(map,str1[j],str2[j],NSTRIP*NSECTOR,values,runnum,&firsttime);
        for(i=0; i<NSECTOR; i++)
        {
          for(k=0; k<NSTRIP; k++)
	  {
            if(values[NSTRIP*i+k] != 0) tch[i][j][k] = values[NSTRIP*i+k];
          } 
        }
      }
    }

    first=0;
  }

  iw = jw; /* gets BOS array address */

  ind = bosNNlink(iw,"EC  ",sec+1);
  if(ind <= 0) return(-1);

  strip_threshold = threshold;
  bzero((char *)strip,NLAYER*NSTRIP*sizeof(ECStrip));

  nhits = (iw[ind-IDATA]*sizeof(int))/sizeof(ECData);
  ec = (ECDataPtr)&iw[ind];

  for(i=0; i<nhits; i++)
  {
    str   = ec->strip-1;
    layer = ec->layer-1;

    if((energy = (ec->adc - e0[sec][layer][str]) * ech[sec][layer][str]) >= strip_threshold)
    {
      strip[layer][str].energy = energy;
/*
printf("sec,layer,str,energy=%d %d %d %d(%f)\n",sec+1,layer+1,str+1,ec->adc,energy);
*/
      strip[layer][str].time =
        ec->tdc * tch[sec][layer][str] - t0[sec][layer][str] ;
    }

    ec++;
  }

  return(ind);
}


/* ecpeak.c - looking for peaks in particular layer

  input:  strip[].energy - strip energy (MeV)
          strip[].time   - strip time (ns)

  output: npeak  - the number of peaks obtained
          peak   - peaks information
*/

static int peak_compare(ECPeak *i, ECPeak *j)
{
  if (i->energy < j->energy) return (1);
  if (i->energy > j->energy) return (-1);
  return (0);
}

int ecpeak_(float *threshold, ECStrip strip[36], ECPeak peak[18])
{
  return(ecpeak(*threshold, strip, peak));
}

int ecpeak(float threshold, ECStrip strip[36], ECPeak peak[18])
{
  int i,strip1,nstrip,npeak;
  float str,sumadc,sumtdc,rstrip;

  bzero((char *)peak, 18*sizeof(ECPeak));

  npeak  = -1;
  strip1 = 0;
  nstrip = 0;
  str  = 0.0;
  sumadc = 0.0;
  sumtdc = 0.0;
  rstrip = 0.0;

  for(i=0; i<NSTRIP; i++)
  {
    if(strip[i].energy > strip_threshold)
    {
      if(strip1 == 0) strip1 = i;
      nstrip++;
      sumadc = sumadc + strip[i].energy;
      str  = str  + (i+0.5)*strip[i].energy;
      rstrip = rstrip + (i+0.5)*(i+0.5)*strip[i].energy;
      sumtdc = sumtdc + strip[i].time;
      if(i >= (NSTRIP-1) || strip[i+1].energy <= strip_threshold)
      {
        npeak++;
        str = str/sumadc;

        peak[npeak].strip1 = strip1;
        peak[npeak].stripn = nstrip;
        peak[npeak].coord  = str;
        peak[npeak].energy = sumadc;
        peak[npeak].time   = sumtdc/nstrip;

        rstrip = rstrip/sumadc - str*str;
        rstrip = (rstrip > (1./12.)) ? rstrip : (1./12.);
        peak[npeak].width = sqrt(rstrip);
        strip1 = 0;
        nstrip = 0;
        str  = 0.0;
        sumadc = 0.0;
        sumtdc = 0.0;
        rstrip = 0.0;
      }
    }
  }
  npeak++;

  /* sorting output array in decreasing energy order */

  qsort((void *)peak, npeak, sizeof(ECPeak), (int (*) (const void *, const void *))peak_compare);
  npeak = 0;
  while(peak[npeak].energy > threshold) npeak++;

  return(npeak);
}


/* echit.c - looking for hits in particular part ( inner , outer, whole )

  input:  geom   - geometry parameters
          npeak  - the number of peaks obtained
          peak   - peaks information

  output: hit    - hits information
*/

int echit_(int *io, int *sector, int npeak[3], ECPeak peak[3][18], ECHit hit[NHIT])
{
  return(echit((*io)-1, (*sector)-1, npeak, peak, hit));
}

int echit(int io, int sector, int npeak[3], ECPeak peak[3][18], ECHit hit[NHIT])
{
  int i, sec, id, u, v, w, npsble, edge;
  float spread,sum_edge,dltz,rmsi,rmsj,rms2,lu,lv,lw,aa,h,h1,h2;
  float iuv,ivw,iwu,juv,jvw,jwu,ratio,depth,H1;
  static first = 1;
  ECParPtr geom;

  /* geometry constants */

  float EcBsc_r = 510.32;    /* distance from target to forward calorimeter plane (cm) */
  float EcBsc_a = 25.0;      /* angle between beam line and perpendicular to calorimeter (degree) */
  float ylow = -182.974;     /* */
  float yhi = 189.956;       /* */
  float dylow = 0.43708;     /* */
  float dyhi = 0.45419;      /* */
  float thickness = 1.2381;  /* thickness of the one layer (plastic + lead) */
  float tgrho = 1.95325;     /* */
  float surf[2] = {1.,16.};
  float ylow_, yhi_, xlow_, xhi_;
  float EcBsc_d[2], EcBsc_H[2], EcBsc_H1[2], EcBsc_H2[2];

  /* geometry calculations - if first */

  if(first)
  {


    for(i=0; i<2; i++)
    {
      ylow_ = ylow - dylow*(surf[i]-1.);
      yhi_ = yhi + dyhi*(surf[i]-1.);
      xlow_=(ylow_-yhi_)/tgrho;
      xhi_=-xlow_;
      EcBsc_d[i] = (surf[i]-1.)*thickness;
      EcBsc_H[i] = yhi_-ylow_;
      EcBsc_H1[i] = ABS(ylow_);
      EcBsc_H2[i] = yhi_;
      for(sec=0; sec<NSECTOR; sec++)
      {
        geometry[sec][i].edge[0] = sqrt(xlow_*xlow_ + (yhi_-ylow_)*(yhi_-ylow_));
        geometry[sec][i].edge[1] = sqrt((xlow_-xhi_)*(xlow_-xhi_));
        geometry[sec][i].edge[2] = sqrt(xhi_*xhi_ + (yhi_-ylow_)*(yhi_-ylow_));

        geometry[sec][i].h  = EcBsc_H[i];
        geometry[sec][i].h1 = EcBsc_H1[i];
        geometry[sec][i].h2 = EcBsc_H2[i];
        geometry[sec][i].d  = EcBsc_d[i];

        geometry[sec][i].the = EcBsc_a*PI/180.;
        geometry[sec][i].phi = sec*2*PI/6.;

        geometry[sec][i].r   = EcBsc_r;
      }
    }




    for(sec=0; sec<NSECTOR; sec++)
    {

      for(i=0; i<2; i++)
      {
/*
        geometry[sec][i].r   = 510.54;
*/
        geometry[sec][i].a   = 25.0;   /* degrees for sector one */
        geometry[sec][i].v   = 50.098; /* closest vertical distance to beam */
/*
        geometry[sec][i].phi = sec*2*PI/6.;
        geometry[sec][i].the = geometry[sec][i].a*PI/180.;
*/
      }

      ratio = ( geometry[sec][1].edge[0] / geometry[sec][0].edge[0] +
                geometry[sec][1].edge[1] / geometry[sec][0].edge[1] )/2.;
      depth = geometry[sec][0].r * (ratio - 1);
/*
      geometry[sec][0].d = depth;
      geometry[sec][1].d = (8./5.) * depth;
*/
      H1 = (geometry[sec][0].r*sin(geometry[sec][0].a*PI/180.) - geometry[sec][0].v) /
              cos(geometry[sec][0].a*PI/180.);
      geometry[sec][0].H1 = H1;         /* inner */
      geometry[sec][1].H1 = ratio * H1; /* outer */
    }

    first = 0;
  }

  geom = &geometry[sector][io]; /* set geometry pointer */

/*
printf("geom: %d %d %f %f %f\n",io+1,sector+1,geom->edge[0],geom->edge[1],geom->edge[2]);
*/


  /* number->cm for coord and width */

  for(edge=0; edge<3; edge++)
  {
    for(id=0; id<npeak[edge]; id++)
    {
      peak[edge][id].coord = peak[edge][id].coord * geom->edge[edge] / 36.;
      peak[edge][id].width = peak[edge][id].width * geom->edge[edge] / 36.;
      peak[edge][id].tmp = 0.0;
    }
  }

  npsble = -1;

  for(u=0; u<npeak[0]; u++)
  {
    for(v=0; v<npeak[1]; v++)
    {
      for(w=0; w<npeak[2]; w++)
      {

        dltz = peak[0][u].coord/geom->edge[0] +
               peak[1][v].coord/geom->edge[1] +
               peak[2][w].coord/geom->edge[2];

        spread = ( peak[0][u].width +
                   peak[1][v].width +
                   peak[2][w].width ) *2.*sqrt(12.);

        sum_edge = geom->edge[0] + geom->edge[1] + geom->edge[2];

        if(ABS(dltz-2.)*sum_edge < spread)
	{
          npsble++;
          if(npsble == NHIT) return(-1);

          if(npsble == 20) /* temporary for compartibility !!! */
          {
            printf("eclib: too many possible hits\n");
            return(-1);
          }

          hit[npsble].sector = sector + 1; /* in hit[] sector = 1..6 */
          hit[npsble].layer = io + 1;      /* in hit[] io = 1..2 */

          hit[npsble].i = geom->h *
                          ( peak[0][u].coord/geom->edge[0] -
                            peak[1][v].coord/geom->edge[1] -
                            peak[2][w].coord/geom->edge[2] ) / 2. +
                          geom->h2;

          hit[npsble].j = geom->edge[1] *
                          ( peak[2][w].coord/geom->edge[2] -
                            peak[1][v].coord/geom->edge[1] ) / 2.;

          hit[npsble].k = geom->d;

/* we are not using di, dj and width ????? */

/* Stepan's - never working for "standart" option, because of iterr=1
          hit[npsble].di = sqrt( peak[0][u].width * peak[0][u].width / geom->edge[0] * geom->edge[0] +
                                 peak[1][v].width * peak[1][v].width / geom->edge[1] * geom->edge[1] +
                                 peak[2][w].width * peak[2][w].width / geom->edge[2] * geom->edge[2] ) *
                           geom->h / 2.;
          hit[npsble].dj = sqrt( peak[2][w].width * peak[2][w].width / geom->edge[2] * geom->edge[2] +
                                 peak[1][v].width * peak[1][v].width / geom->edge[1] * geom->edge[1] ) *
                           geom->edge[1] / 2.;
          hit[npsble].width = sqrt( hit[npsble].di*hit[npsble].di + hit[npsble].dj*hit[npsble].dj );
*/

/* Serguei's
          lu = geom->edge[0];
          lv = geom->edge[1];
          lw = geom->edge[2];
	  h = sqrt(lu*lu-lv*lv/4.);
          h1 = geom->H1;
          h2 = h - h1;
          aa = lv/2.;
          iuv = h*(peak[0][u].coord/lu - h1/h);
          juv = aa*(2.-peak[0][u].coord/lu - 2.*peak[1][v].coord/lv);
          ivw = h*(1.+h2/h-peak[2][w].coord/lw-peak[1][v].coord/lv);
          jvw = aa*(peak[2][w].coord/lw-peak[1][v].coord/lv);
          iwu = iuv;
          jwu = aa*(peak[0][u].coord/lu-2.+2.*peak[2][w].coord/lw);
          rmsi = ((iuv-hit[npsble].i)*(iuv-hit[npsble].i) +
                  (ivw-hit[npsble].i)*(ivw-hit[npsble].i) +
                  (iwu-hit[npsble].i)*(iwu-hit[npsble].i))/3.;
          rmsj = ((juv-hit[npsble].j)*(juv-hit[npsble].j) +
                  (jvw-hit[npsble].j)*(jvw-hit[npsble].j) +
                  (jwu-hit[npsble].j)*(jwu-hit[npsble].j))/3.;
          rms2 = rmsi + rmsj;
          hit[npsble].di = sqrt(rmsi);
          hit[npsble].dj = sqrt(rmsj);
          hit[npsble].width = sqrt(rms2);
*/

          hit[npsble].energy = peak[0][u].energy +
                               peak[1][v].energy +
                               peak[2][w].energy;
          hit[npsble].peak1[0] = u+1; /* for FORTRAN: +1 !!! */
          hit[npsble].peak1[1] = v+1;
          hit[npsble].peak1[2] = w+1;
          peak[0][u].tmp = peak[0][u].tmp + hit[npsble].energy;
          peak[1][v].tmp = peak[1][v].tmp + hit[npsble].energy;
          peak[2][w].tmp = peak[2][w].tmp + hit[npsble].energy;
/*
printf("1: io,sector,npsble,i,j=%d %d %d %f %f %f %f %f %f %f %f %f %f\n",
            io+1,sector+1,npsble+1,peak[0][u].coord,peak[1][v].coord,peak[2][w].coord,hit[npsble].i,hit[npsble].j,hit[npsble].k,
            hit[npsble].di,hit[npsble].dj,hit[npsble].width,hit[npsble].energy);
*/

        }
      }
    }
  }

  return(npsble+1);
}


/* eccorr.c - attenuation correction

  input:  geom   - geometry parameters
          npeak  - the number of peaks obtained
          peak   - peaks information

  output: hit    - hits information
*/

static int hit_compare(ECHit *i, ECHit *j)
{
  if (i->energy < j->energy) return (1);
  if (i->energy > j->energy) return (-1);
  return (0);
}

int eccorr_(float *th, int *def_atten, int *io, int *sector, int npeak[3], ECPeak peak[3][18], int *npsble, ECHit hit[NHIT])
{
  return(eccorr(*th, *def_atten, (*io)-1, (*sector)-1, npeak, peak, *npsble, hit));
}
int eccorr(float th, int def_atten, int io, int sector, int npeak[3], ECPeak peak[3][18], int npsble, ECHit hit[NHIT])
{
  static float atten[NSECTOR][2][3][NSTRIP];
  static int first=1;
  ECParPtr geom;

  int i, j, k, l, ith, axis, ihit, peakid, npks, nhit, firsttime;
  float energy, attn, lat, path[3], fraction[3], *atlen[3];
  char *map_atten = "EC_ATTEN";
  char *str1[2] = {"inner","outer"};
  char *str2[3] = {"u","v","w"};

  if(first)
  {
    printf("eclib: def_atten=%d\n",def_atten);
    for(j=0; j<2; j++)
    {
      for(l=0; l<3; l++)
      {
        for(i=0; i<NSECTOR; i++)
	{
          for(k=0; k<NSTRIP; k++)
	  {
            atten[i][j][l][k] = 376.0;
	  }
	}
      }
    }

    if(!def_atten)
    {
      dir = getenv("CLAS_PARMS");
      for(j=0; j<2; j++)
      {
        for(l=0; l<3; l++)
        {
          sprintf(map,"%s/Maps/%s.map",dir,map_atten);
          printf("eclib: reading file %s\n",map);
          map_get_float(map,str1[j],str2[l],NSTRIP*NSECTOR,values,runnum,&firsttime);
          for(i=0; i<NSECTOR; i++)
          {
            for(k=0; k<NSTRIP; k++)
            {
              if(values[NSTRIP*i+k] != 0) atten[i][j][l][k] = values[NSTRIP*i+k];
            }
          }
        }
      }
    }

    first = 0;
  }

  geom = &geometry[sector][io]; /* set geometry pointer */

  for(l=0; l<3; l++) atlen[l] = atten[sector][io][l];

  /* sorting output array in decreasing energy order */

  qsort((void *)hit, npsble, sizeof(ECHit), (int (*) (const void *, const void *))hit_compare);
  nhit = 0;
  while(hit[nhit].energy > th && nhit < NHIT && nhit < npsble) nhit++;

  /* loop for all hits */

  for(ihit=0; ihit<nhit; ihit++)
  {
    ecpath(geom,hit[ihit].i,hit[ihit].j,path);
/*
printf("2: I,J - U,V,W=%f %f %f %f %f\n",hit[ihit].i,hit[ihit].j,path[0],path[1],path[2]);
*/
    energy = hit[ihit].energy;
    hit[ihit].energy = 0.;
    hit[ihit].time   = 0.;
    hit[ihit].width  = 0.;

    /* loop for 3 axis */

    npks = 0;
    for(axis=0; axis<3; axis++)
    {
      npks++;
      peakid = hit[ihit].peak1[axis] - 1; /* F -> C !!! */
      fraction[axis] = energy / peak[axis][peakid].tmp;

      lat = 0.;
      for(ith=0; ith<peak[axis][peakid].stripn; ith++)
      {
        lat = lat + atlen[axis][peak[axis][peakid].strip1+ith];
      }
      lat = lat / peak[axis][peakid].stripn;
      attn = exp(-path[axis]/lat);
/*
printf("3: axis,lat,dst=%d %f %f\n",axis+1,lat,path[axis]);
*/
      hit[ihit].energy = hit[ihit].energy + fraction[axis] *
                         peak[axis][peakid].energy / attn;
      hit[ihit].time = hit[ihit].time + peak[axis][peakid].time;

      hit[ihit].uvw2[axis] = peak[axis][peakid].width;
      hit[ihit].width = hit[ihit].width + peak[axis][peakid].width;
    }
    hit[ihit].width = hit[ihit].width / 3;
/*
printf("4: ecfit_peaks_hits (%d)l,s,energy,width=%d %d %f %f\n",ihit+1,io+1,sector+1,hit[ihit].energy,hit[ihit].width);
*/
    npks = MAX(npks,1);
    hit[ihit].time = hit[ihit].time / npks;

    ecxyz(geom,hit[ihit].i,hit[ihit].j,hit[ihit].k,&hit[ihit].x);
/*
printf("5: ecfit_peaks_hits (%d)   l,s,i,j,k,x,y,z=%d %d %f %f %f %f %f %f\n",ihit+1,io+1,sector+1,
                                 hit[ihit].i,hit[ihit].j,hit[ihit].k,hit[ihit].x,hit[ihit].y,hit[ihit].z);
*/
  }

  return(nhit);
}


/* ecbos.c - filling BOS bank ECHB by all information ( all io and sectors )

  input:  nhits, hit

*/

int ecbos_(int *nhits, ECHitPtr hit)
{
  return(ecbos(*nhits, hit));
}

int ecbos(int nhits, ECHitPtr hit)
{
  ECHitPtr hitin, hitout;
  int nr, ncol, ind, i;

  nr = 0;
  ncol = (sizeof(ECHit)+3)/4;
  ind = bosNNcreate(iw,"ECHB",nr,ncol,nhits);
  if(ind <= 0) return(-1);

  hitin = hit;
  hitout = (ECHitPtr)&iw[ind];
  for(i=0; i<nhits; i++)
  {
    bcopy((char *)hitin, (char *)hitout, sizeof(ECHit));
    hitin++;
    hitout++;
  }
  return(ind);
}


/* ecshower.c - looking for showers using inner & outer parts
                and filling BOS bank ECRB

  input:  nhits, hit

*/

int ecshower_(int *nhits, ECHitPtr hit)
{
  return(ecshower(*nhits, hit));
}

int ecshower(int nhits, ECHitPtr hit)
{
  ECShower shower[NSHOWER];
  ECHitPtr hitin[NHIT], hitout[NHIT];
  int sec, k, l1, l2, in, out, ish, ncol, nrow, nr, ind;
  float tmp, sigin, sigout, sig, dist, expN, prob;

  tmp = 4.0*sqrt(0.693147); /* alog(2.) = 0.693147 */

  ish = 0;
  for(sec=1; sec<=NSECTOR; sec++)
  {

    l1 = l2 = 0;
    for(k=0; k<nhits; k++)
    {
      if(hit[k].sector == sec)
      {
        if(hit[k].layer == 1)
        {
          hitin[l1++] = &hit[k];
        }
        else
        {
          hitout[l2++] = &hit[k];
        }
      }
    }

/* doesn't work realy: width always = 0 because of bug in ec_dalitz !!!

    for(in=0; in<l1; in++)
    {
      sigin = hitin[in]->width / tmp;
      for(out=0; out<l2; out++)
      {
        sigout = hitout[out]->width / tmp;
        sig = sigin*sigin + sigout*sigout;
        dist = (hitin[in]->i - hitout[out]->i) * (hitin[in]->i - hitout[out]->i) +
               (hitin[in]->j - hitout[out]->j) * (hitin[in]->j - hitout[out]->j);
        if(sig > 0.0)
        {
          expN = MAX(-dist/sig,-70.);
          prob = exp(expN);
        }
        else
        {
          prob = 1.0;
        }
printf("shower1: sec,width,i,j=%d %f %f %f\n",sec,hitin[in]->width,hitin[in]->i,hitin[in]->j);
printf("shower2: sec,width,i,j=%d %f %f %f\n",sec,hitout[out]->width,hitout[out]->i,hitout[out]->j);
printf("shower3: sig,dist,expN,prob=%f %f %f %f\n",sig,dist,expN,prob);

      }
    }
*/


    l1 = MIN(l1,l2);
    k = 0;
    for(in=0; in<l1; in++)
    {
      for(out=0; out<l1; out++)
      {
/*
printf("shower4: s,e_in,e_out=%d %f %f\n",sec,hitin[in]->energy,hitout[out]->energy);
*/
        shower[ish].e_in = hitin[in]->energy;           /* energy found for the inner layer */
        shower[ish].e_out = hitout[out]->energy;        /* energy found for the outer layer */
        shower[ish].dE_in = hitin[in]->denergy;         /* error on the energy found for the inner layer */
        shower[ish].dE_out = hitout[out]->denergy;      /* error on the energy found for the outer layer */
        shower[ish].t_in = hitin[in]->time;             /* time found for the inner layer */
        shower[ish].t_out = hitout[out]->time;          /* time found for the outer layer */
        shower[ish].dt_in = hitin[in]->dtime;           /* error on the time found for the inner layer */
        shower[ish].dt_out = hitout[out]->dtime;        /* error on the time found for the outer layer */
        shower[ish].i_in = hitin[in]->i;                /* sector rectangular coordinate for the inner layer */
        shower[ish].j_in = hitin[in]->j;                /* sector rectangular coordinate for the inner layer */
        shower[ish].i_out = hitout[out]->i;             /* sector rectangular coordinate for the outer layer */
        shower[ish].j_out = hitout[out]->j;             /* sector rectangular coordinate for the outer layer */
        shower[ish].di_in = hitin[in]->di;              /* sector rectangular coordinate error, inner layer */
        shower[ish].dj_in = hitin[in]->dj;              /* sector rectangular coordinate error, inner layer */
        shower[ish].di_out = hitout[out]->di;           /* sector rectangular coordinate error, outer layer */
        shower[ish].dj_out = hitout[out]->dj;           /* sector rectangular coordinate error, outer layer */
        shower[ish].x_in = hitin[in]->x;                /* lab coordinate, inner layer */
        shower[ish].y_in = hitin[in]->y;                /* lab coordinate, inner layer */
        shower[ish].z_in = hitin[in]->z;                /* lab coordinate, inner layer */
        shower[ish].x_out = hitout[out]->x;             /* lab coordinate, outer layer */
        shower[ish].y_out = hitout[out]->y;             /* lab coordinate, outer layer */
        shower[ish].z_out = hitout[out]->z;             /* lab coordinate, outer layer */
        shower[ish].dx_in = hitin[in]->dx;              /* lab coordinate error, inner layer */
        shower[ish].dy_in = hitin[in]->dy;              /* lab coordinate error, inner layer */
        shower[ish].dz_in = hitin[in]->dz;              /* lab coordinate error, inner layer */
        shower[ish].dx_out = hitout[out]->dx;           /* lab coordinate error, outer layer */
        shower[ish].dy_out = hitout[out]->dy;           /* lab coordinate error, outer layer */
        shower[ish].dz_out = hitout[out]->dz;           /* lab coordinate error, outer layer */
        shower[ish].u2_in = hitin[in]->uvw2[0];         /* second moment of u inner hit pattern */
        shower[ish].v2_in = hitin[in]->uvw2[1];         /* second moment of v inner hit pattern */
        shower[ish].w2_in = hitin[in]->uvw2[2];         /* second moment of w inner hit pattern */
        shower[ish].u2_out = hitout[out]->uvw2[0];      /* second moment of u outer hit pattern */
        shower[ish].v2_out = hitout[out]->uvw2[1];      /* second moment of v outer hit pattern */
        shower[ish].w2_out = hitout[out]->uvw2[2];      /* second moment of w outer hit pattern */
        shower[ish].u3_in = hitin[in]->uvw3[0];         /* third moment of u inner hit pattern */
        shower[ish].v3_in = hitin[in]->uvw3[1];         /* third moment of v inner hit pattern */
        shower[ish].w3_in = hitin[in]->uvw3[2];         /* third moment of w inner hit pattern */
        shower[ish].u3_out = hitout[out]->uvw3[0];      /* third moment of u outer hit pattern */
        shower[ish].v3_out = hitout[out]->uvw3[1];      /* third moment of v outer hit pattern */
        shower[ish].w3_out = hitout[out]->uvw3[2];      /* third moment of w outer hit pattern */
        shower[ish].i2 = 0.;          /* second moment of overall shower, sector coordinates */
        shower[ish].j2 = 0.;          /* second moment of overall shower, sector coordinates */
        shower[ish].i3 = 0.;          /* third moment of overall shower, sector coordinates */
        shower[ish].j3 = 0.;          /* third moment of overall shower, sector coordinates */
        shower[ish].istat = sec;                        /* status word */
        if((++ish) > NSHOWER)
        {
          printf("eclib: too many showers\n");
          goto write;
        }

/* TAK CHTO BEREM PERVII VSTRECHNII : */
        if((++k) >= l1)
        {
          /* BEREM PERVII VSTRECHNII */
          goto nextsector;
        }
      }
    }
nextsector:
    ;
  }

write:

  nr = 0;
  ncol = (sizeof(ECShower)+3)/4;
  nrow = ish;
  ind = bosNNcreate(iw,"ECRB",nr,ncol,nrow);
  if(ind <= 0) return(-1);

  bcopy((char *)&shower[0].e_in, (char *)&iw[ind], nrow*ncol*sizeof(int));

  return(ind);
}


/*********************************************************
                some useful utilities
**********************************************************/


/* ecpath.c

  di - I coordinate
  dj - J coordinate

 */

int ecpath(ECParPtr geom, float di, float dj, float path[3])
{
  float lu, lv, lw, h, h1, du, dv, dw;

  lu = geom->edge[0];
  lv = geom->edge[1];
  lw = geom->edge[2];
  h = sqrt(lu*lu-lv*lv/4.);
  h1 = geom->h1;

  du = (di/h + h1/h)*lu;
  dv = lv - h1/2./h*lv - dj - lv/2./h*di;
  dw = lw/lv*dj - lw/2./h*di - h1/2./h*lw + lw;

  path[0] = du/lu*lv - (lv-dv);
  path[1] = dv/lv*lw - (lw-dw);
  path[2] = dw/lw*lu - (lu-du);

  return(0);
}


/* ecxyz.c

  *di - I coordinate
  *dj - J coordinate
  *dk - K coordinate

 */

int ecxyz(ECParPtr geom, float di, float dj, float dk, float xyz[3])
{
  int j;
  float phi, the, pv[3];
  static float rot[3][3];

  the = geom->the;
  phi = geom->phi;

  rot[0][0] = cos(the) * cos(phi);
  rot[0][1] = -sin(phi);
  rot[0][2] = sin(the) * cos(phi);
  rot[1][0] = cos(the) * sin(phi);
  rot[1][1] = cos(phi);
  rot[1][2] = sin(the) * sin(phi);
  rot[2][0] = -sin(the);
  rot[2][1] = 0.;
  rot[2][2] = cos(the);

  for(j=0; j<3; j++) pv[j] = 0.;
  for(j=0; j<3; j++)
  {
    pv[j] = pv[j] + rot[j][0]*di;
    pv[j] = pv[j] + rot[j][1]*dj;
    pv[j] = pv[j] + rot[j][2]*dk;
  }

  xyz[0] = pv[0] + geom->r * sin(the) * cos(phi);
  xyz[1] = pv[1] + geom->r * sin(the) * sin(phi);
  xyz[2] = pv[2] + geom->r * cos(the);
 
/* note for reverse xyz->ijk
 *     do j=1,3
 *       do i=1,3
 *         pv(j)=rot(i,j)*P(i)
 *       enddo
 *     enddo
 * since this is a unitary matrix
 */

  return(0);
}







