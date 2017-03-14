/* -=======================================================- *
$Id: txt2part.c,v 1.18 1999/04/02 20:13:48 manak Exp $
$Author: manak $
$Revision: 1.18 $
$Date: 1999/04/02 20:13:48 $
* -=======================================================- */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include <ntypes.h> 
#include <bostypes.h> 
#include <pid.h> 
#include <particleType.h>
#include <pdgutil.h>
#include <scalers.h>

/* declare the bos common */
BOSbank bcs_;
BOSbank wcs_;
void parseTargetPosition(char *str,double *zmin,double *zmax);
void PrintUsage(char *processName);

int main(int argc,char *argv[]){
  
  int OutputUnitNo = 7,
    MaxBanks = 1000,
    i,
    group = 0,
    NEvnt = 0,  
    NPart,
    DiscardLines = 0,
    MakeMCTAGR = 0;
  float Charge[NUM_PARTICLES];
  char *argptr;
  char *BOSoutfile = "part.evt";
  char  ChJunk[200];
  char  out[300];
  clasPART_t *PART=NULL;
  part_t part[PART_SIZE];
  head_t head[30];
  clasHEAD_t * HEAD = NULL;
  float Egamma, Eelectron, toffset; /*stuff for making TAGR*/
  int RunNo = 0;
	int nOut = 0;
  double zmin = 0.0, zmax = 0.0;

  for(i=1; i < argc; ++i){
    argptr = argv[i];
    if( *(argptr = argv[i])  == '-') {
 
      switch(*(++argptr)) {
      case 'h':
        PrintUsage(argv[0]);
        exit(1);
	break; 
      case 'z':
	parseTargetPosition(++argptr,&zmin,&zmax);
	break;
      case 'o':
	if(*(++argptr))
	  BOSoutfile = argptr;
	else
	  PrintUsage(argv[0]);
	break;
      case 'd':
	DiscardLines = atoi(++argptr);
	break;
      case 'T':
	MakeMCTAGR = 1;
	break;
      case 'R':
	RunNo = atoi(++argptr);
	break;
      }
    }
  }

  if(OutputUnitNo){
    fprintf(stderr,"Output file: %s\n",BOSoutfile);
    unlink(BOSoutfile);
    sprintf(out, "OPEN AUTOOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", BOSoutfile);
    if (!fparm_c(out)) {
      fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0], out, strerror(errno));	 
      exit(1);
    }
  }
 
  bnames_(&MaxBanks); 
  initbos();
  configure_banks(stderr,0);
  
  bankList(&bcs_, "E=", "HEADPARTTAGR");  
  
  /* get rid of the first three lines of GenBod spew */
  for(i=0; i<DiscardLines; i++){
    fgets(ChJunk, 200, stdin); 
  }
  if(MakeMCTAGR){ /*set up TAGR stuff*/
    fill_ebin_map();
    fill_E_T_map();
  }

  while(fscanf(stdin, "%i", &NPart) == 1){
    memset((void *)&(part[0]), 0, PART_SIZE*sizeof(part_t));
    memset((void *)&(head[0]), 0, 30*sizeof(head_t));
    
    if(MakeMCTAGR){
      if(fscanf(stdin, "%f %f %g", &Eelectron, &Egamma, &toffset,1) == 3){
	make_mctagr(&bcs_,Eelectron,Egamma,toffset,1);
      }
      else{
	fprintf(stderr, "txt2part: has encountered a format input error in event %d.\n,", NEvnt+1);
	exit(1);
      } 
    }
    for(i=0; i<NPart; i++){
      if(fscanf(stdin, "%i %f %f %f %f", &part[i].pid, &part[i].p.t, &part[i].p.space.x, 
		&part[i].p.space.y, &part[i].p.space.z) == 5 && 
	 fscanf(stdin, "%f %f %f", &part[i].vert.x, &part[i].vert.y, &part[i].vert.z) == 3){
	part[i].q = gchrg(part[i].pid);
      }
      else{
	fprintf(stderr, "txt2part: has encountered a format input error in event %d.\n,", 
		NEvnt+1);
	exit(1);
      }
    }
    if(NPart){
      if((HEAD = makeBank(&bcs_, "HEAD", 0, sizeof(head_t)/4, 1)) && 
	 (PART = makeBank(&bcs_, "PART", group, sizeof(part_t)/4, NPart))){
	time_t secs;
	time(&secs);
	head[0].nevent = ++NEvnt;
	head[0].nrun = RunNo;
	head[0].type = -4;
	head[0].time = secs;
	HEAD->head[0] = head[0];
	for(i=0; i < NPart; i++) {
	  PART->part[i] = part[i]; 
	}
      }
      putBOS(&bcs_, OutputUnitNo, "E");
	nOut++;
    }
    /* tidy up */
    dropAllBanks(&bcs_,"E");
    cleanBanks(&bcs_);
  }
 putBOS(&bcs_, OutputUnitNo, "0");
  sprintf(out, "CLOSE AUTOOUTPUT UNIT=7");
  fparm_c(out);
    fprintf(stderr,"# of events written = %d\n",nOut);
  return 0;
}
void PrintUsage(char *processName)
{
  fprintf(stderr, "   \n");
  fprintf(stderr, "%s: text to PART BOS bank converter.\n", processName);
  fprintf(stderr, "input:\tstdin text file of particle 4-vectors\n");
  fprintf(stderr, "output:\tBOS file with PART bank\n");
  fprintf(stderr, "options:\n");
  fprintf(stderr, "\t-h          \tprint this Help message.\n");
  fprintf(stderr, "\t-o<outfile> \tOutput BOS file name.\n");
  fprintf(stderr, "\t-d<int>     \tDiscard the first n lines.\n");
  fprintf(stderr, "\t-T          \tmake TAGR bank: requires additional txt\n");
  fprintf(stderr, "\t-R<int>     \tmake HEAD bank for a given run-number\n");
  fprintf(stderr, "example:\n");
  fprintf(stderr, "\ttxt2part -oEventGenerator.evt < EventGenerator.dat\n");
  fprintf(stderr, "The format of the input text data is as follows:\n");
  fprintf(stderr, "\tN\n");
  fprintf(stderr, "\tE_electron, E_gamma, T_offset  /*only used if -T option chosen*/\n");
  fprintf(stderr, "\tpid Pt Px Py Pz\n");
  fprintf(stderr, "\tVx Vy Vz\n");
  fprintf(stderr, "\t      ...\n");
  fprintf(stderr, "Where N is the number of particles in an event, pid is the geant particle id, \nPt is the time component of the four-momentum, and Px, Py, Pz are the space \ncomponents.  Vx, Vy, and Vz are the vertex location of the particle.\nE_electon is the incident beam energy on the radiator, E_photon is the energy\n of the photon, and T_offset is the time offset due to the event vertex not \nbeing at the origin (ie toffset = -z/c).  E_electron,E_gamma, and T_offset \nare only used if the -T option is used.\n");
}


void parseTargetPosition(char *str,double *zmin,double *zmax)
{
  double z1 = 0,z2 = 0;
  char *word;
  if (nchar(str,',') == 1) {
    /* user has specified a 2-vector */
    word = strtok(str,",");
    z1 = atof(word);
    word = strtok(NULL,",");
    z2  = atof(word);
  }
  else if (nchar(str,',') == 0) {
    z1 = z2 = atof(str);
  }
  else {
    //  cerr << "error parsing target position:" << str << endl;
  }
  //  cerr << "Target position set to: " << z1 << " -> " << z2 << endl;
  *zmin = z1;
  *zmax = z2;
}







