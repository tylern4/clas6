// 
// This series of functions allow you to use the g11
// trigger efficiency table.  It allows for reading the
// table, and returning the specified efficiency, and it 
// allows for checking that efficiency against a random number.
//

#ifndef triggerEfficiency_H
#define triggerEfficiency_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>



float EFFICIENCY[3][6][48][30]; // charge/sector/tofpaddle/phibin

//
// The InitEfficiency() function reads the trigger efficiency table 
// into memmory, and allows for use of the Efficiency() function.
//
// It also seeds the random number generator for the checktrigeff() 
// function.
//
//
//  int run for g11 = 0
//  Default values are run = 0 and random seed = 1.
//
//

extern "C"{
   
   void InitTriggerEfficiency(int run, int randseed);
   
   void inittriggerefficiency_(int *run, int *randseed);
   
   float TriggerEfficiency(int particleindex, double p[3], int& paddle);
   
   float triggerefficiency_(int *particleindex, float *px, float *py, float *pz, int *paddle);

   int checktrigeff(float efficiency);

   int checktrigeff_(float *efficiency);
}

   
void inittriggerefficiency_(int *run, int *randseed)
{
  // fortran wrapper
  InitTriggerEfficiency(*run,*randseed);
}

float triggerefficiency_(int *particleindex, float *px, float *py, float *pz, int *paddle)
{
  // fortran wrapper
  float teff;
  double p[3];
  p[0]=*px;
  p[1]=*py;
  p[2]=*pz;
  teff=TriggerEfficiency(*particleindex,p,*paddle);
  return(teff);
}

int checktrigeff_(float *efficiency)
{
  // fortran wrapper
  int trigger;
  trigger=checktrigeff(*efficiency);
  return(trigger);
}


void InitTriggerEfficiency(int run = 0, int randseed = 1){

  int sec, tof, philow, phihigh, phimid;
  float EFF;
  int phi;
  char particle[256];
  int particleindex = 0;
  char *dir;
  char triggerMap[512];
  FILE * inFile;

  /* Get the map name */
  dir=getenv("CLAS_PACK");
  if(run==0)
  {
    /* Open the efficiency map associated with g11 */
    sprintf(triggerMap,"%s/utilities/CMUcuts/g11_trigger_efficiency_byparticle.txt",dir);
    printf("Opening trigger efficiency map %s:\n",triggerMap );
  }
  else
  {
    printf ("%s \n", "");
    printf ("%s %d\n", "Not set up to get trigger efficiencies for run ",run);
    printf ("%s \n", "");
    exit(-1);
  }

  inFile = fopen(triggerMap,"r");

  if (inFile==NULL) 
  {
    printf("Attempting to open %s\n",triggerMap);
    perror ("Error opening file");
  }

  while(fscanf (inFile, "%s", &particle) == 1)
  {
    fscanf (inFile, "%d", &sec);
    fscanf (inFile, "%d", &tof);
    fscanf (inFile, "%d", &philow);
    fscanf (inFile, "%d", &phihigh);
    fscanf (inFile, "%d", &phimid);
    fscanf (inFile, "%E", &EFF);

    if(strcmp(particle,"proton") == 0)       {particleindex=0;}
    else if(strcmp(particle,"piplus") == 0)  {particleindex=1;}
    else if(strcmp(particle,"piminus")== 0)  {particleindex=2;}
	
    //Adjust phibin values for array
    phi = ((int)(philow + 30)*1000)/2000;

    //Set array efficiencies
    EFFICIENCY[particleindex][sec-1][tof-1][phi] = EFF;

  }
  // close file
  fclose(inFile);

  // seed random number with input
  srand(randseed);

}



//
// This function returns the efficiency from the trigger
// efficiency table.  You must pass in the 3 momentum, particle name,
// and the TOF paddle from that particle.
//
//
// RETURNS EFFICIENCY AS A FLOAT
//

float TriggerEfficiency(int particleindex, double p[3], int& paddle){

  double phiradians = 0;
  double sectorphi = 0;
  double pi = 4*atan(1);
  float eff;
/*  int particleindex = 0;*/
  double phideg = 0;
  int sector = 0;
  int verbose= 0;
  
/*  if(strcmp(particle,"proton") == 0)       {particleindex=0;}
    else if(strcmp(particle,"piplus") == 0)  {particleindex=1;}
    else if(strcmp(particle,"pimimus") == 0) {particleindex=2;}
    else if(strcmp(particle,"kplus") == 0)   {particleindex=1;}
    else if(strcmp(particle,"kmimus") == 0)  {particleindex=2;} */
  


  phiradians = atan2(p[1],p[0]);
  phideg = (phiradians/(2*pi))*360;
  
  // Use phi to get sector number and set phi in sector:
  if((phideg >= -30.000) && (phideg < 30.000))    {sector = 1; sectorphi = phideg;}
  if((phideg >= 30.000) && (phideg < 90.000))     {sector = 2; sectorphi = phideg - 60.;}  
  if((phideg >= 90.000) && (phideg < 150.000))    {sector = 3; sectorphi = phideg - 120.;}
  if((phideg >= 150.000) && (phideg < 180.000))   {sector = 4; sectorphi = phideg - 180.;}
  if((phideg >= -180.000) && (phideg < -150.000)) {sector = 4; sectorphi = phideg + 180.;}
  if((phideg >= -150.000) && (phideg < -90.000))  {sector = 5; sectorphi = phideg + 120.;}
  if((phideg >= -90.000) && (phideg < -30.000))   {sector = 6; sectorphi = phideg + 60.;}

  //Convert float phi to phibin
  int phibin;
  phibin = ((int)((sectorphi + 30)*1000))/2000;
  
  // get efficiency from array
  eff =   EFFICIENCY[particleindex][sector-1][paddle-1][phibin];

  /* Dump info if verbose mode */
  if(verbose)
  {
    printf("particle:\t %d\n", particleindex);
    printf("p x,y,z:\t %f, %f, %f\n",p[0],p[1],p[2]);
    printf("pmag:\t\t %f \n",sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]));
    printf("phi:\t \t%f\n",phideg);
    printf("sectorphi:\t %f\n",sectorphi);
    printf("sector:\t \t%d\n",sector);
    printf("paddle:\t \t%d\n",paddle);
    printf("efficiency:\t %f\n",eff);
    printf("efficiency:\t %f %f %f\n",EFFICIENCY[0][1-1][15-1][0],EFFICIENCY[0][1-1][16-1][0],EFFICIENCY[0][1-1][17-1][0]);
  }

  return eff;
}


// 
//
//  This function allows you to pass in 
//  an efficiency for a triggering particle
//  and it will throw and psuedo-random number
//  and determine if statistically you would
//  have seen that particle.  Returns a 1 for 
//  a particle you would have seen, and a 0 
//  otherwise.
// 
//  YOU MUST SEED THE rand() prior to this call!
//  This is done in the InitEfficiency() function
//

int checktrigeff(float efficiency){
  int trigger = 0;
  double x;

  //Generate Random Number normalized from 0->1
  x = rand()/((double)RAND_MAX + 1);
  
  // check random number vs. efficiency for particle.
  if( x < efficiency){  trigger=1;}

  return trigger;
}


#endif
