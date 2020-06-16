#ifndef _SCALEMCEFFIENCY_H
#define _SCALEMCEFFIENCY_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* scaleMCefficiency.h
 *
 * ****************************************************
 *
 * scaleMCefficiency
 *
 * 01/27/06  M. Bellis
 *
 * 
 * It was found by the CMU group, using an empirical efficiency
 * calculation, that the efficency for protons and pions does not
 * quite match up with with the efficiency of the CLAS detector
 * does not quite match up with that of the gsim analysis package. 
 * Mostly for high momentum tracks. 
 *
 * This code is designed to scale the efficency of gsim, which is found
 * to be higher. The number returned is data/MC. For a given Monte Carlo 
 * event, one could throw a random number between 0-1 and accept the
 * event if it is lower than the scaling factor found in this 
 * momentum bin.
 *
 * particle - 0 (proton), 1 (pi+) and 2 (pi-)
 * sector - CLAS sector (1-6)
 * pmag - magnitude of the momentum
 * cos - cosine(theta) of the momentum
 * phi - phi of the track as measured in CLAS. The code will translate phi in the lab
 *       frame to phi in a sector. 
 * effCut - minimum efficiency requried for both data and Monte Carlo (default=0.0)
 * verbose - option that produces some diagnostic output on the screen.
 *
 * ****************************************************
 *
 * InitScaleMC
 *
 * 01/27/06  M. Bellis
 *
 * run - integer to represent some run, or different cuts for a given run. 
 *       0 - g11a, The current cuts were motivated by differences in an empirical 
 *                 efficiency calculation between data and MC which indicated discrepencies 
 *                 in the forward region at the high momentum.
 *       1 - g11a, much like 0, but using a smoothing algorithm on the final histograms
 *           used in the study.
 *       2 - g11a, tighter phi cuts used in the study
 *       3 - g11a, only scales protons below 450 MeV
 *       4 - g11a, scales low momentum protons (<450MeV) and foward going tracks (cos(theta)>0.9)
 *
 *
 * verbose - option that produces some diagnostic output on the screen.
 *
 * ****************************************************
 *
 * GLOBAL VARIABLES
 *
 * scaleMCefficiency - array of the scale factors binned by particle, sector, pmag, phi and cosTheta
 * dataEff - array of the data efficiency binned by particle, sector, pmag, phi and cosTheta
 * mcEff - array of the Monte Carlo efficiency binned by particle, sector, pmag, phi and cosTheta
 *                    
 * pmagwidth = 0.01 
 * phiwidth  = 0.1  
 * coswidth  = 0.01  - widths of the bins of scaleMC efficiency. Note that the map can have coarser binning
 *                     and multiple bins will just be filled with the same value. There would be issues
 *                     if the map tries to bin finer than these settings.
 * 
 * 
 */
/*float scaleMCefficiency(int particle, int sector, float pmag, float phi, float cos, float effCut=0.0, int verbose=0);*/

float effRatio[3][6][400][12][200]; /* particle/sector/pmag/phi/cosTheta */
float dataEff[3][6][400][12][200]; /* particle/sector/pmag/phi/cosTheta */
float mcEff[3][6][400][12][200]; /* particle/sector/pmag/phi/cosTheta */
float pmagwidth; /* GeV */
float phiwidth; /* rad */
float coswidth; /*  */

extern "C"{

  void InitScaleMC(int run, int verbose);
  
  void initscalemc_(int *run, int *verbose);
  
  float scaleMCefficiency(int particle, int sector, float pmag, float phi, float cos, float effCut=0.0, int verbose=0);

  float scalemcefficiency_(int *particle, int *sector, float *pmag, float *phi, float *cos, float *effCut, int *verbose);

  float oneSectorPhi(int sec, float phi);

  float onesectorphi_(int *sec, float *phi);
}


void initscalemc_(int *run, int *verbose)
{
  //fortran wrapper
  InitScaleMC(*run,*verbose);
}


float scalemcefficiency_(int *particle, int *sector, float *pmag, float *phi, float *cos, float *effCut, int *verbose)
{
  //fortran wrapper
  float scaled_eff;
  scaled_eff=scaleMCefficiency(*particle,*sector,*pmag,*phi,*cos,*effCut,*verbose);
  return(scaled_eff);
}


void InitScaleMC(int run = 0, int verbose = 0)
{

  int particle;
  int sector;
  float pmaglo, pmaghi;
  float coslo, coshi;
  float philo, phihi;
  float er;
  FILE * inFile;
  char mceffmap[256];
  char *dir;
  int option;
  int icos, ipmag, iphi;
  int ipmaghi;
  int iphihi;
  int icoshi;
  int ipmaglo;
  int iphilo;
  int icoslo;
  float data, mc;

  /* Get the map name */
  dir=getenv("CLAS_PACK");
    /* Open the efficiency map associated with g11 */
    sprintf(mceffmap,"%s/utilities/CMUcuts/scaleMCefficiency.%d.MAP",dir, run);
    if(verbose) printf("Trying to open %s\n",mceffmap);

  inFile = fopen(mceffmap,"r");
  if (inFile==NULL) 
  {
    printf("Attempting to open %s\n",mceffmap);
    perror ("Error opening file");
    printf ("%s %d\n", "Not set up to get trigger efficiencies for run ",run);
    printf ("%s \n", "");
    printf ("%s %d\n", "Will be unable to scale anything.... ");
  }


  /* Set default map values to 0 */
  if(verbose) printf("Initializing the map.....\n");
  for(int i0=0;i0<3;i0++) /* particles */
  {
    for(int i1=0;i1<6;i1++) /* sectors */
    {
      for(int i2=0;i2<400;i2++) /* pmag */
      {
        for(int i3=0;i3<12;i3++) /* phi */
        {
          for(int i4=0;i4<200;i4++) /* costheta */
          {
            effRatio[i0][i1][i2][i3][i4] = 1.0; 
            dataEff[i0][i1][i2][i3][i4] = 1.0; 
            mcEff[i0][i1][i2][i3][i4] = 1.0; 
          }
        }
      }
    }
  }
  if(verbose) printf("Initialized the map.....\n");

  if (inFile!=NULL) 
  {
  if(verbose) printf("Opened %s\n",mceffmap);
  /* Read everything in from the map */
  while(fscanf (inFile, "%d", &option) == 1)
  {
    fscanf (inFile, "%d", &particle);
    fscanf (inFile, "%d", &sector);
    fscanf (inFile, "%f", &pmaglo);
    fscanf (inFile, "%f", &pmaghi);
    fscanf (inFile, "%f", &philo);
    fscanf (inFile, "%f", &phihi);
    fscanf (inFile, "%f", &coslo);
    fscanf (inFile, "%f", &coshi);
    fscanf (inFile, "%f", &data);
    fscanf (inFile, "%f", &mc);
    fscanf (inFile, "%f", &er);

    pmagwidth = pmaghi - pmaglo;
    coswidth = coshi - coslo;
    phiwidth = phihi - philo;

    /* Translate momentum to a discrete bin */
    ipmaghi = (int)(0.5+pmaghi/pmagwidth);
    icoshi = (int)(0.5+(coshi+1)/coswidth);
    iphihi = (int)(0.5+(phihi+0.5)/phiwidth);

    ipmaglo = (int)(0.5+pmaglo/pmagwidth);
    icoslo = (int)(0.5+(coslo+1)/coswidth);
    iphilo = (int)(0.5+(philo+0.5)/phiwidth);

    /* Dump a whole lot of information if verbose is 2 */
    if(verbose==2) 
    {
      printf("%f %f\t",pmaglo,pmaghi);
      printf("%f %f\t",philo,phihi);
      printf("%f %f\n",coslo,coshi);
      printf("%d %d\t",ipmaglo,ipmaghi);
      printf("%d %d\t",iphilo,iphihi);
      printf("%d %d\n",icoslo,icoshi);
      printf("%f %f %f\n",philo,philo+0.5,(philo+0.5)/phiwidth);
      printf("%f %f %f\n",phihi,philo+0.5,(phihi+0.5)/phiwidth);
    }

    /* 
     * Fill the map, the efficiency numbers that we have read in typically
     * need to fill a block of the map that go between the low and high 
     * range of each variable that we read in. 
     */
    for(ipmag=ipmaglo;ipmag<ipmaghi;ipmag++)
    {
      for(iphi=iphilo;iphi<iphihi;iphi++)
      {
        for(icos=icoslo;icos<icoshi;icos++)
        {
          if(verbose==2) printf("%d\t%d\t%d\t%d\t%d\t%f\n",particle, sector, ipmag, iphi, icos, er);
          effRatio[particle][sector][ipmag][iphi][icos] = er;
          dataEff[particle][sector][ipmag][iphi][icos] = data;
          mcEff[particle][sector][ipmag][iphi][icos] = mc;
        }
      }
    }


  }
  /* close file */
  fclose(inFile);
  }
  /*
   * Look back over the map and extend the efficiency at the lower edge of 
   * cos(theta) to try and fill in the holes.
   */
  int nedge=5;

  for(int i0=0;i0<3;i0++) /* particles */
  {
    for(int i1=0;i1<6;i1++) /* sectors */
    {
      for(int i2=0;i2<400;i2++) /* pmag */
      {
        for(int i3=0;i3<12;i3++) /* phi */
        {
          for(int i4=0;i4<200-nedge;i4++) /* costheta */
          {
            if ((effRatio[i0][i1][i2][i3][i4]==0.)&&(effRatio[i0][i1][i2][i3][i4+nedge-1]!=0.)&&(effRatio[i0][i1][i2][i3][i4+nedge]!=0.)) {
              effRatio[i0][i1][i2][i3][i4]=0.5*(effRatio[i0][i1][i2][i3][i4+nedge-1]+effRatio[i0][i1][i2][i3][i4+nedge]);
            } 
            if ((effRatio[i0][i1][i2][i3][i4]!=0.)&&(effRatio[i0][i1][i2][i3][i4-1]==0.)&&(effRatio[i0][i1][i2][i3][i4-2]!=0.)) {
              effRatio[i0][i1][i2][i3][i4-1]=0.5*(effRatio[i0][i1][i2][i3][i4]+effRatio[i0][i1][i2][i3][i4-2]);
            }
          }
        }
      }
    }
  }
}

/*****************************************************/
float scaleMCefficiency(int particle, int sector, float pmag, float labphi, float cos, float effCut, int verbose)
{
  float sectorphi;
  int icos, ipmag, iphi;
  float scale = 1.0;

  /* Convert lab phi to a phi defined in the sector */
  sectorphi = oneSectorPhi(sector, labphi); 

  /* Get the bin corresponding to momentum */
  ipmag = (int)(pmag/pmagwidth);
  iphi = (int)((sectorphi+0.5)/phiwidth);
  icos = (int)((cos+1)/coswidth);

  /* get efficiency from array */
  if(iphi>=0 && ipmag>=0 && icos>=0)
  {
    scale =   effRatio[particle][sector-1][ipmag][iphi][icos];
  }

  /* Dump info if verbose mode */
  if(verbose)
  {
    printf("particle: %d\tsector: %d\n", particle ,sector);
    printf("pmag: %f\t\tbin: %d\n",pmag, ipmag);
    printf("cos: %f\t\tbin: %d\n",cos, icos);
    printf("sectorphi: %f\tbin: %d\n",sectorphi, iphi);
    printf("scale: %f\n", scale);
  }

  /* If data or Monte Carlo efficiency is below the passed in effCut, 
   * return a 0. Imposes some minimum efficiency on the analysis 
   */
  if(dataEff[particle][sector-1][ipmag][iphi][icos] < effCut ||
      mcEff[particle][sector-1][ipmag][iphi][icos] < effCut )
  {
    return 0.0;
  }

  /* Do not scale if MC is more efficient than data */
  if(scale>1.0) scale = 1;

  return scale;
}

#endif
