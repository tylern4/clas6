/*
 * makebos.cc
 */

static const char sccsid[] = "@(#)"__FILE__"\t1.69\tCreated 3/26/97 20:31:35, \tcompiled "__DATE__;

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <math.h>
#include <mcgen.h>

extern "C" {

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <bosddl.h>
#include <particleType.h>
#include <utility.h>

void ProgramEnd();
void bnames_(int *);
void bprnt_(BOSbank *,char *,int);
void bosta_();


}

#define BUFSIZE 10000

void pParticle(Particle_t,ThreeVector,FourVector);
float theta(float QSQ,float W,float E);
float eprime(float theta,float Qsq,float E);
int GetData(int,int);
void makeHead(int);
float Q(Particle_t pid);

void Usage(char *);


int main (int argc, char *argv[])
{

  int max = 0;
  int npart = 0;
  int Nevents = 0;
  int Nwrite = 0;
  int more = 1;
  
  int maxbanks = 1000;
  int RunNo = 0;
  
  char *outfile = NULL; 
  char out[100], mess[100];

  for (int iarg = 1; iarg < argc; ++iarg) {
    char *argptr = argv[iarg];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) {
	
      case 'h':
	Usage (argv[0]);
	exit(0);
	break; 
      case 'R':
	RunNo = atoi(++argptr);
	break;

      case 'n':
	npart = atoi(++argptr);
	break;

      case 'o':
	outfile =  *(++argptr) ? argptr : "/dev/fd/1";
	fprintf(stderr,"Output file: %s\n",outfile);
	unlink(outfile);
	sprintf(out, "OPEN BOSOUTPUT UNIT=7 FILE=\"%s\" WRITE STATUS=NEW RECL=3600", outfile);
	if (!fparm_c(out)) {
	  fprintf(stderr,"%s: Unable to open file \'%s\': %s\n\n",argv[0],out,strerror(errno));	 
	  exit(1);
	}
	break; 
      case 'M':
	max = atoi (++argptr);
	break;
      }
    }
  }
  bnames_(&maxbanks); 
  initbos();
  configure_banks(stderr,0);





  if (npart) {

 
    while (max ? more && (Nevents < max ) : more) {

      if ( more = GetData(npart,RunNo)) {
	if (RunNo)
	  bankList(&bcs_,"E=","PARTHEAD");
	else
	  bankList(&bcs_,"E=","PART");

	Nevents++;
	if (outfile) {
	  putBOS(&bcs_,7,"E");
	  Nwrite++;
	}
      }

        
    }
    if (outfile) {
       /*close file*/
        sprintf(mess,"CLOSE BOSOUTPUT UNIT=7");
        fparm_c(mess);
    }

    cerr << "Number of events read: " << Nevents << endl;
    cerr  << "Number of events written: " << Nwrite << endl;


  }
  else
    cerr << "You must set the number of particles with the -n option" << endl;

   
}

void ProgramEnd()
{
  ;
}

void Usage (char *ProcessName)
{
  cerr << ProcessName << " [options] << inputfile" << endl;
  cerr << "\t-R#\tRun number" << endl;
  cerr << "\t-n#\t# of particles/event" << endl;
  cerr << "\t-ofilename\tOutput file" << endl;
  cerr << "\t-M#\t# of events to read (default = all events in input file)" << endl;

}


void pParticle(Particle_t type,ThreeVector v,FourVector p)
{

  cout << (int) type << " ";
  cout <<   v.X() << " " << v.Y() << " " << v.Z() << " ";
  cout << p.T() << " " << p.X() << " " << p.Y() << " " << p.Z() << endl;

}

float Q(Particle_t pid)
{
  float ret = 0.0;
  switch (pid) {
  case PiPlus:
  case Proton:
  case KPlus:
  case Positron:
    ret = 1;
    break;
  case PiMinus:
  case Electron:
  case KMinus:
    ret = -1;
    break;
  case Gamma:
  case Neutron:
  case KLong:
  case KShort:
  case Pi0:
    ret = 0;
    break;
  }
  return(ret);
}

int GetData(int npart,int RunNo)
{
  int ret = 1;
  int n;
   int i = 0;
   int pT;
   float px,py,pz,t;
  float x,y,z;
   Particle_t partType;
   clasPART_t  *PART = NULL;


   makeHead(RunNo);
   PART=(clasPART_t *) makeBank(&bcs_, "PART", MONTECARLO_PART, sizeof(part_t)/sizeof(int), npart); 
 
  while (ret && (i < npart)) {
 
  if ( (cin >> pT >> x >> y >> z >> t>> px >> py >> pz )) {

    ThreeVector prod(x,y,z);
    FourVector p(px,py,pz,t);
  
  
      switch (i) {
      case 0:
      default:

	PART->part[i].pid =  pT;
	PART->part[i].vert.x =     x;
	PART->part[i].vert.y =    y;
	PART->part[i].vert.z =    z;
	PART->part[i].p.space.x = px;
	PART->part[i].p.space.y = py;
	PART->part[i].p.space.z = pz;
	PART->part[i].p.t =  t;
	PART->part[i].q = Q((Particle_t) pT);
	
       }
      i++;
  }
  else {
    ret = 0;
  }
    
    }

  return(ret);
  
}



void makeHead(int RunNo)
{

  static int nevent = 0;

#define h HEAD->head[0]

  clasHEAD_t *HEAD;
  time_t secs;

  if (RunNo) {

    time(&secs);

    /* make the HEAD bank */
    HEAD = (clasHEAD_t *) makeBank(&bcs_,"HEAD",0,sizeof(head_t)/sizeof(int),1);
    h.version = 0;
    h.nrun = RunNo;
    h.nevent = ++nevent;
    h.time = secs;
    h.type = -2;
    h.roc = 0;
    h.evtclass = 1;
  }
}
  
  
  
