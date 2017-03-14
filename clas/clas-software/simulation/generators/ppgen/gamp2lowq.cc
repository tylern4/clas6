/*
  gamp2lowq: convert gamp format to CLAS PART Bank

 */


extern "C" {
  
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <particleType.h>
#include <kinematics.h>
#include <pdgutil.h>
#include <pid.h>
#include <utility.h>
#include <printBOS.h>
#include <ec.h>
#include <PartUtil.h>
#include <map_manager.h>
#include <time.h>



#define MELECTRON .00051
  
  /* ----------- Function prototypes ---------------- */
void ctrlCHandle(int);
void PrintUsage(char *processName);
void bnames_(int *);
  /* declare the bos common */
BOSbank bcs_;
  
}  

#include <iostream.h>
#include <kinematics_o.h>
#include <clasPART_o.h>
#include <pputil.h>
#include <clasEvent.h>
clasPART_t *make_pizero_part_bank(int GroupNo, int newpart);
part_t makeBeam(int pid, vector4_t p );
void printgamp(part_t *part);
float beamP(int run);
double Number(double x);
double randm (double low, double high);
float gaussian_gen();

#include <matrix.h>
#include <Vec.h>
#include <lorentz.h>
#include <event.h>

#include <plib.h>

extern particleDataTable PDGtable;



/* --------------------------------------------------- */

void PrintUsage(char *processName)
{
  cerr << "Usage: " << processName << " [-M] file1 [file2] etc....\n\n";
  cerr << "  Options:\n\n";
  cerr << "     -M[#]\t\tUse only [#] number of events\n";
  cerr << "     -p[#]\t\tUse this number part bank for output file\n";
  cerr << "     -r[#]\t\tUse [#] for runno (default runno = 1)\n";
  cerr << "     -o<filename>\tUse <filename> for output BOS file\n";
  cerr << "     -t[#]\t\tUse [#] for trigger bit identification (default = 1)\n";
  cerr << "     -T\t\t\tPut Beam particle in TAGR bank (photon use only!)\n";
  cerr << "     -V\t\t\tUse debugging mode\n";
  cerr << "     -h\t\t\tPrint this message\n\n";
  exit(0);
}



main(int argc,char **argv)
{
  int i;
  char *argptr, *outfile = NULL;
  int Nevents = 0, Nfile = 0;

  clasOutput coutput;
  int OutputUnitNo = 9;

  int ret;
  ios::sync_with_stdio();
  int partno = 0;
  int verbose = 0;
  int Verbose = 0;
  int debug=0;
  int makeTAGR=0,offset=0;
  clasLOWQ_t *LOWQ,*LOWQ1;
  clasPART_t *PART; 
  clasHEAD_t *HEAD = NULL;
  clasTAGR_t *TAGR = NULL;
  event evt;
  int maxEvents = 0;


  int ElectronRun = 0;
  int CurrentRun = 0;

  int MaxBanks = 1000;


  double zmin = -109.0,zmax = -91.0;
  double z = 0.0;
  char *word;

  int runno = 1;
  int trigbit = 1;

  int num_particles=0,nwrite=0;
  int pid = 0,pcharge = 0;
  float momx=0.0,momy=0.0,momz=0.0,E=0;

  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
  for (i=1; i<argc; i++) {
    argptr = argv[i];
    if (*argptr == '-') {
      argptr++;
      switch (*argptr) { 

      case 'z':
	if (word = strtok(++argptr,",")) {
	  zmin =  atof(word);
	  if (word = strtok(NULL," ")) {
	    zmax  = atof(word);
	  }
	}
	cerr << "z min: \t" << zmin << "\tz max:\t" << zmax << endl;
	break;


      case 'v':
	verbose = 1;
	break; 
      case 'V':
	Verbose = 1;
	debug = 1;
	break;
      case 'r':
	runno = atoi(++argptr);
	break;
      case 'h':
	PrintUsage(argv[0]);
	break;      
      case 'M':
	maxEvents = atoi(++argptr);
	break;
      case 'o':
	
	if(*(++argptr)) {
	  unlink(argptr);
	  coutput.open(argptr,OutputUnitNo);
	  cerr << "Output file: " << argptr << endl;
	}
	break;

      default:
	cerr << "Unrecognized argument:\t" << argptr << endl;
	PrintUsage(argv[0]);
	break;
      }
    }
  }
  PDGtable.initialize();
 
  /*initialize bos and hbook*/
  bnames_(&MaxBanks);
  initbos();
  configure_banks(stderr,Verbose);

  while (!(cin >> evt).eof()  && (maxEvents ? Nevents < maxEvents : 1)) {
    fourVec scatE =  evt.getPartPFinal("e",-1,2);
    double sigmaE;
    double newE,newP;
    if (verbose)
      cout << scatE.lenSq() << " " << scatE.V().r() << " " << scatE.V().cosTheta() << " " << scatE.V().phi()/M_PI << endl;

    sigmaE = .03/sqrt(scatE.t());
    newE = gauss(scatE.t(),sigmaE);
    newP = sqrt(newE * newE - ELECTRON_MASS * ELECTRON_MASS);
    

  
    LOWQ = (clasLOWQ_t *)makeBank(&bcs_,"LOWQ",0,sizeof(lowq_t)/sizeof(int),1);
    LOWQ1 = (clasLOWQ_t *)makeBank(&bcs_,"LOWQ",1,sizeof(lowq_t)/sizeof(int),1);
    TAGR = (clasTAGR_t *)makeBank(&bcs_,"TAGR",0,sizeof(tagr_t)/sizeof(int),1);

    LOWQ->lowq[0].pid = Electron;
    LOWQ->lowq[0].vert.x = 0.0;
    LOWQ->lowq[0].vert.y = 0.0; 
    LOWQ->lowq[0].vert.z = 0.0;
    
    LOWQ->lowq[0].p.space.x = scatE.V().x();
    LOWQ->lowq[0].p.space.y = scatE.V().y(); 
    LOWQ->lowq[0].p.space.z = scatE.V().z();
    LOWQ->lowq[0].p.t = scatE.t();

    LOWQ1->lowq[0].pid = Electron;
    LOWQ1->lowq[0].vert.x = 0.0;
    LOWQ1->lowq[0].vert.y = 0.0; 
    LOWQ1->lowq[0].vert.z = 0.0;
    
    LOWQ1->lowq[0].p.space.x = sin(scatE.V().theta()) * cos(scatE.V().phi()) * newP;
    LOWQ1->lowq[0].p.space.y =  sin(scatE.V().theta()) * sin(scatE.V().phi()) * newP;
    LOWQ1->lowq[0].p.space.z = newP * scatE.V().cosTheta();
    LOWQ1->lowq[0].p.t = newE;


    HEAD = (clasHEAD_t *)makeBank(&bcs_,"HEAD",0,sizeof(head_t)/sizeof(int),1);
    HEAD->head[0].version = 1105;
    HEAD->head[0].nrun = runno;
    HEAD->head[0].nevent = Nevents+1;
    HEAD->head[0].type = 1;
    HEAD->head[0].evtclass = 15;
    HEAD->head[0].trigbits = trigbit;
    HEAD->head[0].time = time(NULL);


    TAGR->tagr[0].erg=evt.beam().get4P().t();
    TAGR->tagr[0].tpho = TAGR->tagr[0].ttag = 0.0;
    TAGR->tagr[0].stat=15;
    
    {
      int npart = 0;
      int n = 0;
      int nbaryon = 0;
      int nmeson = 0; 
      list<particle> all = evt.f_particles();
      list<particle> mesons = evt.f_mesons();
      list<particle> baryons = evt.f_baryons();
      list<particle>::const_iterator p = mesons.begin();
      list<particle>::const_iterator b = baryons.begin();
      list<particle>::const_iterator a = all.begin();
      threeVec vertex(0.0,0.0,randm(zmin,zmax));

      while( p != mesons.end() ) {
	nmeson++;
	p++;
      }
      if (verbose)
	cout << "# mesons:\t" << nmeson << "\t";

      while( b != baryons.end() ) {
	if (verbose)
	  cout << "Baryon:\t" << b->Name() << endl;

	nbaryon++;
	b++;
      }
      if (verbose)
	cout << "# baryons:\t" << nbaryon << endl; 

      while( a != all.end() ) {
	int id;
	id = (int) name2id(a->Name(),a->Charge());
	if (verbose)
	  cout << a->Name() << " " << a->J() << " "  << (int) id << endl;;
	a++;
	switch (id) {
	case PiPlus:
	case PiMinus:
	case Proton:
	  npart++;
	  break;
	}
      }
      PART = (clasPART_t *)makeBank(&bcs_,"PART",0,sizeof(part_t)/sizeof(int),npart);
      a = all.begin();
      while( a != all.end() ) {

	Particle_t pid;
	int id;
	id = (int) name2id(a->Name(),a->Charge());
	pid = (Particle_t) id;
	switch (pid) {
	case PiPlus:
	case PiMinus:
	case Proton:
	  PART->part[n].pid = pid;
	  PART->part[n].q = a->Charge();
	  PART->part[n].p.space.x = a->get3P().x();
	  PART->part[n].p.space.y = a->get3P().y();	 
	  PART->part[n].p.space.z = a->get3P().z();
	  PART->part[n].p.t = a->get4P().t();

	  // vertex 

	  PART->part[n].vert.x = vertex.x();
	  PART->part[n].vert.y = vertex.y();
	  PART->part[n].vert.z = vertex.z();
	  
	  n++;
	  break;
	}
	a++;
      }	
	  

    }
  

      

    if (coutput.status()) {
      coutput.write(&bcs_,"HEADLOWQPARTTAGR");
    }

    Nevents++;
  }
  dropAllBanks(&bcs_,"E");
  cleanBanks(&bcs_);

  cerr << "Total # of events processed:\t" << Nevents << endl;
  if(coutput.status()) {
    coutput.write(&bcs_,"0");
    coutput.close();
  }
}

void ctrlCHandle(int x)
{
  signal(SIGINT,ctrlCHandle);
  signal(SIGHUP,ctrlCHandle);
}

float gaussian_gen()
{
 double r1,r2=9999,g1=1;

 while (r2>g1)
   {
    r1=randm(-3,3);
    g1=exp(-1*(r1*r1)/2)/sqrt(2.*M_PI);
    r2=randm(0,1);
   }
 return(r1);
}

double randm (double low, double high)
{
  return ((high - low) * drand48 () + low);
}

/* end file */
