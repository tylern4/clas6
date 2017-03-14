/*
  txtEvent: convert gamp format to CLAS PART/MC Banks

*/
using namespace std;
#include <unistd.h>
#include <string>

extern "C"
{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <ntypes.h>
#include <bostypes.h>
#include <clas_cern.h>
#include <particleType.h>
#include <kinematics.h>
#include <pdgutil.h>
#include <pid.h>
#include <scalers.h>
#include <utility.h>
#include <printBOS.h>
#include <ec.h>
#include <PartUtil.h>
#include <map_manager.h>
#include <time.h>


  typedef struct
  {
    double ebin;
    double flux;
  } fluxBin_t;


#define MELECTRON .00051

  int maxFail = 0;

  /* ----------- Function prototypes ---------------- */
  void ctrlCHandle (int);
  void PrintUsage (char *processName);
  /* declare the bos common */
  BOSbank bcs_;
  int nchar (char *, char);
}

#include <txtEvent.h>
#include <particleData.h>

#include <iostream>
#include <kinematics_o.h>
#include <clasPART_o.h>
extern particleDataTable PDGtable;
double getFlux (list < fluxBin_t > Flux, double ebeam);
clasPART_t *make_pizero_part_bank (int GroupNo, int newpart);
part_t makeBeam (int pid, vector4_t p);
void printgamp (part_t * part);
float beamP (int run);
double Number (double x);
double randm (double low, double high);
void parseTargetPosition (char *str, double *zmin, double *zmax);
double gaussian (double xmean, double xwidth);
int g2l (int pid);
/*#include <matrix.h>
  #include <Vec.h>
  #include <lorentz.h>*/




/* --------------------------------------------------- */

void
PrintUsage (char *processName)
{
  cerr << "Usage: " << processName << " [-M] file1 [file2] etc....\n\n";
  cerr << "  Options:\n\n";
  cerr << "     -M[#]\t\tUse only [#] number of events\n";
  cerr << "     -p[#]\t\tUse this number part bank for output file\n";
  cerr << "     -r[#]\t\tUse [#] for run number (default = 1)\n";
  cerr << "     -o<filename>\tUse <filename> for output BOS file\n";
  cerr <<
    "     -t[#]\t\tUse [#] for trigger bit identification (default = 1)\n";
  cerr << "     -T\t\t\tPut Beam particle in TAGR bank (photon use only!)\n";
  cerr << "     -z[#,#]\t\tDistribute vertex-z is given range\n";
  cerr << "     -S[#]\t\tDistribute vertex-xy with this sigma\n";
  cerr << "     -V\t\t\tUse debugging mode\n";
  cerr << "     -h\t\t\tPrint this message\n\n";
  exit (0);
}



clasTGEO_t *
makeTGEO (int RunNo)
{
  char *dir, map[100];
  int firsttime;
  float T[3];
  clasTGEO_t *TGEO;

  /* Get target position from geometry bank */
  dir = (char *) getenv ("CLAS_PARMS");
  sprintf (map, "%s/Maps/GEOMETRY.map", dir);
  map_get_float (map, "target", "position", 3, T, RunNo, &firsttime);
  TGEO = (clasTGEO_t *) makeBank (&wcs_, "TGEO", 0, 6, 1);
  TGEO->tgeo[0].x = T[0];
  TGEO->tgeo[0].y = T[1];
  TGEO->tgeo[0].z = T[2];
  return (TGEO);
}




main (int argc, char **argv)
{
  FILE *fileptr = NULL;
  int c;
  extern char *optarg;
  extern int optind;
  int max = 0;

  clasTGEO_t *TGEO;

  char *outfile = NULL;
  int Nevents = 0, Nfile = 0;
  ifstream weightfile;
  char out[100], mess[100];
  int ret;
  ios::sync_with_stdio ();
  int partno = 0;
  int verbose = 0;
  int Verbose = 0;
  int debug = 0, trigbit = 1;
  int makeTAGR = 0, offset = 0;
  int addBeam = 0;

  int ElectronRun = 0;
  int CurrentRun = 0;

  clasPART_t *PART = NULL;
  clasHEAD_t *HEAD = NULL;
  clasTAGR_t *TAGR = NULL;
  clasMCTK_t *MCTK = NULL;
  clasMCVX_t *MCVX = NULL;
  double cx = 0.0, cy = 0.0, cz = 0.0, mass;

  txtEvent e;
  int num_particles = 0, nwrite = 0;
  int pid = 0, pcharge = 0;
  float momx = 0.0, momy = 0.0, momz = 0.0, E = 0;

  int carbon = 0;
  int iron = 0;

  int makeMC = 0;
  int makePart = 0;

  char *fluxFile;
  ifstream fluxfile;
  list < fluxBin_t > Flux;
  double wt, ebin;
  int useFlux = 0;

  string outbanks ("HEAD");

  double zmin = -20.6, zmax = -14.4;
  double beamWidth = 0.4;
  long int RDMseed = 0;
  int runNo = 0;

  signal (SIGINT, ctrlCHandle);
  signal (SIGHUP, ctrlCHandle);

  while ((c = getopt (argc, argv, "BPCIFvVz:r:hM:S:Tp::t:o:mR:f:")) != -1)
    {
      switch (c)
	{
	case 'B':
	  addBeam = 1;
	  break;
	case 'v':
	  verbose = 1;
	  break;
	case 'V':
	  Verbose = 1;
	  debug = 1;
	  break;
	case 'z':
	  parseTargetPosition (optarg, &zmin, &zmax);
	  break;

	case 'h':
	  PrintUsage (argv[0]);
	  break;
	case 'R':
	case 'r':
	  runNo = atoi (optarg);
	  break;
	case 'M':
	  max = atoi (optarg);
	  break;
	case 'm':
	  makeMC = 1;
	  break;
	case 'S':
	  beamWidth = atof (optarg);
	  break;
	case 'T':
	  makeTAGR = 1;
	  break;
	case 'p':
	  makePart = 1;
	  if (optarg)
	    partno = atoi (optarg);
	  break;
	case 't':
	  trigbit = atoi (optarg);
	  break;
	case 'o':
	  outfile = optarg;
	  unlink (outfile);
	  sprintf (out,
		   "OPEN BOSOUTPUT UNIT=%d FILE=\"%s\" WRITE STATUS=NEW RECL=3600",
		   7, outfile);
	  if (!fparm_c (out))
	    {
	      fprintf (stderr, "%s: Unable to open file \'%s\': %s\n\n",
		       argv[0], out, strerror (errno));
	      exit (1);
	    }
	  break;

	case 'C':
	  carbon = 1;
	  break;
	case 'F':
	  iron = 1;
	  break;
	case 'P':
	  zmin = zmax = -4.5;
	  break;

	case 'f':
	  fluxFile = optarg;
	  fluxfile.open (optarg);
	  useFlux = 1;
	  break;




	default:
	  cerr << "Unrecognized argument:\t" << endl;
	  PrintUsage (argv[0]);
	  break;
	}

    }
  PDGtable.initialize ();
  if (!RDMseed)
    {
      srand48 ((long) time (NULL));
    }
  else
    {
      srand48 (RDMseed);
    }

  if (useFlux)
    {

      // read the flux file

      while (!(fluxfile >> ebin >> wt).eof ())
	{
	  fluxBin_t f;
	  f.ebin = ebin;
	  f.flux = wt;
	  Flux.push_back (f);
	}

    }




  /*initialize bos and hbook */
  initbos ();

  if (makePart)
    outbanks += "PART";
  if (makeMC)
    outbanks += "MCTKMCVX";
  if (makeTAGR)
    {
      makeTGEO (runNo);
      outbanks += "TAGR";
      TGEO = (clasTGEO_t *) getBank (&wcs_, "TGEO");
    }

  while (!(cin >> e).eof () && (max ? Nevents < max : 1))
    {
      int i = 0;
      fourVec beam = e.beam ().get4P ();
      list < particle > l = e.f_particles ();


      if (carbon)
	{
	  double fx = randm (0, 4);
	  int ix = (int) fx;

	  switch (ix)
	    {
	    case 0:
	      zmin = zmax = -11.83;
	      break;
	    case 1:
	      zmin = zmax = -6.93;
	      break;
	    case 2:
	      zmin = zmax = -2.03;
	      break;
	    case 3:
	      zmin = zmax = 2.87;
	      break;
	    }
	}

      if (iron)
	{
	  double fx = randm (0, 2);
	  int ix = (int) fx;

	  switch (ix)
	    {
	    case 0:
	      zmin = zmax = -9.42;
	      break;
	    case 1:
	      zmin = zmax = 0.4;
	      break;
	    }
	}


      double zvert = randm (zmin, zmax);
      double xvert = gaussian (0.0, beamWidth);
      double yvert = gaussian (0.0, beamWidth);


      if (debug)
	cerr << "beginning event " << Nevents + 1 << endl;

      num_particles = l.size ();

      HEAD =
	(clasHEAD_t *) makeBank (&bcs_, "HEAD", 0,
				 sizeof (head_t) / sizeof (int), 1);
      if (makeTAGR)
	{

	  TAGR =
	    (clasTAGR_t *) makeBank (&bcs_, "TAGR", 0,
				     sizeof (tagr_t) / sizeof (int), 1);
	  TAGR->tagr[0].erg = beam.t ();
	  TAGR->tagr[0].stat = 15;
	  TAGR->tagr[0].ttag = TAGR->tagr[0].tpho =
	    (TGEO->tgeo[0].z - zvert) / LIGHT_SPEED;
	  offset = 1;
	}




      if (makePart)
	{
	  list < particle >::iterator p = l.begin ();

	  PART =
	    (clasPART_t *) makeBank (&bcs_, "PART", partno,
				     sizeof (part_t) / sizeof (int),
				     num_particles);

	  /* First particle is the beam */


	  while (p != l.end ())
	    {
	      fourVec v = p->get4P ();
	      if (i == 0)
		{
		  PART->part[0].qtrk =
		    useFlux ? e.weight () * getFlux (Flux,
						     beam.t ()) : e.weight ();
		  PART->part[0].flags = e.code ();
		}

	      PART->part[i].pid = name2id (p->Name (), p->Charge ());
	      PART->part[i].q = p->Charge ();
	      PART->part[i].p.space.x = v.x ();
	      PART->part[i].p.space.y = v.y ();
	      PART->part[i].p.space.z = v.z ();
	      PART->part[i].p.t = v.t ();

	      PART->part[i].vert.x = xvert;
	      PART->part[i].vert.y = yvert;
	      PART->part[i].vert.z = zvert;

	      p++;
	      i++;
	    }
	}
      if (makeMC)
	{
	  list < particle >::iterator p = l.begin ();
	  i = 0;

	  MCTK =
	    (clasMCTK_t *) makeBank (&bcs_, "MCTK", 0, sizeof (mctk_t) / 4,
				     num_particles + addBeam);
	  MCVX =
	    (clasMCVX_t *) makeBank (&bcs_, "MCVX", 0, sizeof (mcvx_t) / 4,
				     num_particles + addBeam);
	  if (addBeam)
	    {
	      num_particles++;
	      fourVec v = beam;
	      cx = v.x () / v.V ().r ();
	      cy = v.y () / v.V ().r ();
	      cz = v.z () / v.V ().r ();
	      mass = ~v;
	      MCTK->mctk[i].cx = cx;
	      MCTK->mctk[i].cy = cy;
	      MCTK->mctk[i].cz = cz;
	      MCTK->mctk[i].pmom = v.V ().r ();
	      MCTK->mctk[i].mass = mass;
	      MCTK->mctk[i].charge = p->Charge ();
	      MCTK->mctk[i].id = g2l (name2id (p->Name (), p->Charge ()));
	      MCTK->mctk[i].beg_vtx = 1;
	      MCTK->mctk[i].end_vtx = 1;
	      MCTK->mctk[i].flag = 1;
	      MCTK->mctk[i].parent = 0;
	      MCVX->mcvx[i].x = xvert;
	      MCVX->mcvx[i].y = yvert;
	      MCVX->mcvx[i].z = zvert;
	      MCVX->mcvx[i].tof = 0.0;
	      MCVX->mcvx[i].flag = 0;
	      i++;
	    }




	  while (p != l.end ())
	    {
	      fourVec v = p->get4P ();
	      cx = v.x () / v.V ().r ();
	      cy = v.y () / v.V ().r ();
	      cz = v.z () / v.V ().r ();
	      mass = ~v;
	      MCTK->mctk[i].cx = cx;
	      MCTK->mctk[i].cy = cy;
	      MCTK->mctk[i].cz = cz;
	      MCTK->mctk[i].pmom = v.V ().r ();
	      MCTK->mctk[i].mass = mass;
	      MCTK->mctk[i].charge = p->Charge ();
	      MCTK->mctk[i].id = g2l (name2id (p->Name (), p->Charge ()));
	      MCTK->mctk[i].beg_vtx = 1;
	      MCTK->mctk[i].end_vtx = 1;
	      MCTK->mctk[i].flag = 1;
	      MCTK->mctk[i].parent = 0;
	      MCVX->mcvx[i].x = xvert;
	      MCVX->mcvx[i].y = yvert;
	      MCVX->mcvx[i].z = zvert;
	      MCVX->mcvx[i].tof = 0.0;
	      MCVX->mcvx[i].flag = 0;
	      i++;
	      p++;

	    }
	}


      HEAD->head[0].version = 1105;
      HEAD->head[0].nrun = runNo ? runNo : e.run ();
      HEAD->head[0].nevent = e.event ();
      HEAD->head[0].type = -2;
      HEAD->head[0].evtclass = 15;
      HEAD->head[0].trigbits = trigbit;
      HEAD->head[0].time = time (NULL);

      if (outfile && num_particles)
	{
	  char *banks = (char *) outbanks.c_str ();
	  putBOS (&bcs_, 7, banks);
	  nwrite++;
	}
      dropAllBanks (&bcs_, "E");
      cleanBanks (&bcs_);
      Nevents++;
    }

  sprintf (mess, "CLOSE BOSOUTPUT", outfile);
  fparm_c (mess);
  cerr << "Total # of events processed:\t" << Nevents << endl;
  cerr << "Total # of events written:\t" << nwrite << endl;
}

void
printgamp (part_t * part)
{
  cout << part->pid << " " << gchrg (part->pid) << " " << Number (part->p.
								  space.
								  x) << " ";
  cout << Number (part->p.space.y) << " " << Number (part->p.space.
						     z) << " " <<
    Number (part->p.t) << endl;
}

void
ctrlCHandle (int x)
{
  signal (SIGINT, ctrlCHandle);
  signal (SIGHUP, ctrlCHandle);
  maxFail = 1;
  fprintf (stderr, "\n\n\t\t\t***  INTERRUPTED!!!  ***\n\n");
}

float
beamP (int run)
{
  char *map = "RUN_CONTROL.map";
  char *subsystem = "beam";
  char *item = "energy";
  int length = 1;
  int firsttime;
  float p;
  char x[1000];
  float ret;


  sprintf (x, "%s/Maps/%s", getenv ("CLAS_PARMS"), map);
  map_get_float (x, subsystem, item, length, &p, run, &firsttime);
  cerr << "BEAM MOMENTUM for Run " << run << " = " << p << " MeV " << endl;
  return (p / 1000.0);
}

double
Number (double x)
{

  return (isnan (x) ? 1000.0 : x);
}

void
parseTargetPosition (char *str, double *zmin, double *zmax)
{
  double z1 = 0, z2 = 0;
  char *word;
  if (nchar (str, ',') == 1)
    {
      /* user has specified a 2-vector */
      word = strtok (str, ",");
      z1 = atof (word);
      word = strtok (NULL, ",");
      z2 = atof (word);
    }
  else if (nchar (str, ',') == 0)
    {
      z1 = z2 = atof (str);
    }
  else
    {
      cerr << "error parsing target position:" << str << endl;
    }
  cerr << "Target position set to: " << z1 << " -> " << z2 << endl;
  *zmin = z1;
  *zmax = z2;
}

double
randm (double low, double high)
{
  return ((high - low) * drand48 () + low);
}

double
gaussian (double xmean, double xwidth)
{
  double r1, r2 = 9999, g1 = 1;

  while (r2 > g1)
    {
      r1 = randm (-3, 3);
      g1 = exp (-1 * (r1 * r1) / 2) / sqrt (2. * M_PI);
      r2 = randm (0, 1);
    }
  return (xwidth * r1 + xmean);
}

int
g2l (int pid)
{
  if (pid == 1)
    {
      return (22);
    }
  else if (pid == 2)
    {
      return (-11);
    }
  else if (pid == 3)
    {
      return (11);
    }
  else if (pid == 4)
    {
      return (12);
    }
  else if (pid == 5)
    {
      return (13);
    }
  else if (pid == 6)
    {
      return (-13);
    }
  else if (pid == 7)
    {
      return (111);
    }
  else if (pid == 8)
    {
      return (211);
    }
  else if (pid == 9)
    {
      return (-211);
    }
  else if (pid == 11)
    {
      return (321);
    }
  else if (pid == 12)
    {
      return (-321);
    }
  else if (pid == 13)
    {
      return (2112);
    }
  else if (pid == 14)
    {
      return (2212);
    }
  else if (pid == 15)
    {
      return (-2212);
    }
  else if (pid == 17)
    {
      return (40221);
    }
  else if (pid == 18)
    {
      return (3122);
    }
  else
    {
      return (0);
    }
}

double
getFlux (list < fluxBin_t > Flux, double ebeam)
{
  double ret = 1.0;
  list < fluxBin_t >::iterator it = Flux.begin ();
  while (it != Flux.end ())
    {
      if (fabs (it->ebin - ebeam) < 0.015)
	return (it->flux);
      it++;
    }
  return (ret);

}


/* end file */
