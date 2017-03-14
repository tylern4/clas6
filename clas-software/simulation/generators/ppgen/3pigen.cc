/*
 * 3pigen.cc  by Mina 
 */

//3pigen -M7 -E5.7 -r4.8,5.52 -z-112.,-92 -j1 -m15000 -L0.8 -U2.4 -t1.7 -G   

static const char sccsid[] = "@(#)"__FILE__"\t1.69\tCreated 3/26/97 20:31:35, \tcompiled "__DATE__;

using namespace std;

#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include <ntypes.h>
#include <Vec.h>
#include <lorentz.h>
#include <particleType.h>
#include <event.h>  
#include <3pigen.h>

extern "C" {
#include <stdio.h>
}

#define SPEED_OF_LIGHT 3.0E8

extern particleDataTable PDGtable;  // inc_derived/particleData.h: class particleDataTable   
// order: In <event.h>: #include <particle.h>  ;  In <particle.h>: #include <particleData.h>
int UseBrem = 0;
int UseRange = 0;
static float pbeamZ = 5.7;
static float EbeamZ = 5.7;
static float Vertex_Z_Center = -102.0;
static float  range0 = 0.0, range1 = 0.0;
static double  brem0 = 4.8,  brem1 = 5.52;
static float targetZ0 = -112.0, targetZ1 = -92.0;
int UseTarget = 0;
int FlatMass = 0;
int Isotropic = 0;
double Slope = 3.0;
Particle_t beamid = Gamma;   // particle_t particleType.h
long int RDMseed = 0;
int printBeam = 1;
int printAll = 0;
int printBaryon = 0;
int txt2part_style = 0;
int gamp = 0;
int dalitz = 0;
int verbose = 0;

int main (int argc, char *argv[])
{
  int mode = 0;
  int newargc = 0;
  char *word;
  char **newargv = (char **) malloc(argc * sizeof(char *));
  int nw = 0,*wlist;

  ios::sync_with_stdio();
  
  newargv[newargc++] = argv[0];	   
   
  for (int i = 0; i < argc; ++i) {
    cerr << argv[i] << " ";
  }
  cerr << endl; 

  PDGtable.initialize();

  if (argc <= 1) {
    MUsage (argv[0]);
    exit(1);
  }
  else {
    int usage = 1;
    for (int iarg = 1; iarg < argc; ++iarg) {
      char *ptr = argv[iarg];
      if (*ptr == '-') {
	ptr++;
	switch (*ptr) {
	case 'h':
	  if (usage) {
	    MUsage (argv[0]);
	    exit(0);
	  }
	  else 
	    newargv[newargc++] = argv[iarg];	     
	  break;
	case 'H':
	  if (usage) {
	    cerr << "\n\nThe Particle Id Table:\n " << endl;
	    PrintParticleID();
	    cerr << "\n\nThe Particle Data Table:\n " << endl;
	    PDGtable.print();
	    cerr << endl;
	    exit(0);
	  }
	  else 
	    newargv[newargc++] = argv[iarg];	     
	  break;
	case 'S':
	  RDMseed = atoi(++ptr);
	  break;
	case 'F':
	  FlatMass = 1;
	  break;
	case 'A':
	  printAll = 1;
	  break;
	case 'j':
	  beamid = (Particle_t) atoi(++ptr);
	  cerr << "Beam type: " << beamid << endl;
	  break;
	case 'r':
	  ptr++;
	  word = strtok(ptr,",");
	  range0 = atof(word);
	  word = strtok(NULL," ");
	  range1 = atof(word);
	  cerr << "Beam range: " << range0 << " --> " << range1 << endl;
	  UseRange = 1;
	  break;
	case 'z':
	  ptr++;
	  if (strlen(ptr)) {
	    word = strtok(ptr,",");
	    targetZ0 = atof(word);
	    word = strtok(NULL," ");
	    targetZ1 = atof(word);
	  }
	  cerr << "target range: " << targetZ0 << " --> " << targetZ1 << endl;
	  UseTarget = 1;
	  break;
	case 'b':
	  ptr++;
	  word = strtok(ptr,",");
	  brem0 = atof(word);
	  word = strtok(NULL," ");
	  brem1 = atof(word);
	  cerr << "Brem range: " << brem0 << " --> " << brem1 << endl;
	  UseBrem = 1;
	  break;
	case 'v':
	  verbose = 1;
	  break;
	case 'I':
	  Isotropic = 1;
	  cerr << "Events will be generated Isotropically." << endl;
	  break;
	  case 'P':
	  /* beam momentum */
	  pbeamZ = atof(++ptr);
	  break;
	case 'E':
	  EbeamZ = atof(++ptr);/* Primary electron beam energy for photon beams */
	  break;
	case 't':
	  Slope = atof(++ptr);
	  cerr << "tslope: " << Slope << endl;
	  break;
	case 'M':
	  mode = atoi (++ptr);
	  usage = 0;
	  break;
	case 'T':
	  txt2part_style = 1;
	  break;
	case 'G':
	  gamp = 1;
	  break;
	case 'd':
	  dalitz = 1;
	  break;
	default:
	  newargv[newargc++] = argv[iarg];
	  break;
	}
      }
    }

    if(!RDMseed){
      srand48((long)time(NULL));  /* initialization functions which should be called before using drand48() */
    }else{
      srand48(RDMseed); 
    }

    switch (mode) {
    case 7:
      cpcmcp(newargc,newargv,beamid,PiPlus,PiMinus,PiPlus);
      break; 
    default:
      MUsage (argv[0]);
      break;
    }
  }
}

/* -------------------------------- beam p -> c+ c+ c- N ------------------------------------- */
  
/*  example gamma p -> Pi+ Pi+ Pi- n */

void cpcmcp(int argc, char *argv[],Particle_t Beam,Particle_t Cplus1,Particle_t Cminus,Particle_t Cplus2){
  
  int 
    icount = 0,
    maxevents = 9999999,
    nevents = 0,
    lfevents = 5000,
    Print = 0,
    debug = 0;

  Particle_t 
    Baryon,
    Target = Proton;

  double 
    masslow = Mass(Cplus1) + Mass(Cminus) + Mass(Cplus2),
    masshigh = 0.,
    exp_tMin,
    exp_tMax,

    LorentzFactor = 0,
    lfmax = 0,
    resonance_mass,
    isobar1_mass;

  float
    tMin,
    tMax,
    expt,
    beamMass,
    baryonMass;

  // include/kinematics.h: vector3_t,vector4_t
  vector3_t 
    vbeam,
    pbeam;
  
  //  inc_derived/Vec.h: threeVec, fourVec	
  threeVec  
    zeroVec = threeVec (0.0, 0.0, 0.0);
  
  fourVec 
    beam,
    target,
    resonance,
    recoil,
    cminus,
    cplus1,
    cplus2,
    isobar1;
  
  lorentzTransform Boost;  // inc_derived/lorentz.h
  
  for (int iarg = 1; iarg < argc; ++iarg) {
    char *ptr = argv[iarg];
    if (*ptr == '-') {
      ptr++;
      switch (*ptr) {
      case 'm':
	ptr++;
	maxevents = atoi (ptr);
	cerr << "maxevents: " << maxevents << "\n";
	break;
      case 'l':
	ptr++;
	lfevents = atoi (ptr);
	cerr << "lfevents: " << lfevents << "\n";
	break;
      case 'L':
	ptr++;
	masslow = atof(ptr);
	cerr << "Lower mass limit: " << masslow << "\n";
	break;
      case 'U':
	ptr++;
	masshigh = atof(ptr);
	cerr << "Highter mass limit: " << masshigh << "\n";
	break;
      case 'p':
	Print = 1;
	break;
      case 'B':
	printBaryon = 1;
	break;
      case 'D':
	debug = 1;
	break;
      case 'h':
	UsageM7(argv[0]);
	return;
      default:
	cerr << "unrecognized argument: " << *ptr << endl;
	break;
      }
    }
  }

  double slope = (Slope > 0.0) ? Slope : 3.0;
  double cpcmcpThreshold = Mass(Cplus1) + Mass(Cminus) + Mass(Cplus2);
  double cpcmThreshold = Mass(Cplus1) + Mass(Cminus); 
  int qtot = Q(Beam) + Q(Proton) - Q(Cplus1) - Q(Cminus) -Q(Cplus2);
  switch (qtot) {
  case 0:
    Baryon = Neutron;
    break;
  case 1:
    Baryon = Proton;
    break;
  default:
    cerr << "illegal charge combination:\ttotal charge  = " << qtot << endl;
    exit(0);
  }

  beamMass = Mass(Beam);
  baryonMass = Mass(Baryon);

  if(masslow < cpcmcpThreshold) masslow = cpcmcpThreshold;

  while (maxevents) {//while maxevents

    /* use real beam distribution */
    generateBeamMCinsideTarget(&vbeam, &pbeam); 

    /* beam and target in lab frame */
    beam = fourVec(sqrt(pow(double(pbeam.x),2.0) + pow(double(pbeam.y),2.0) + pow(double(pbeam.z),2.0) + pow (double(beamMass), 2.0)),threeVec(pbeam.x, pbeam.y, pbeam.z));
    target = fourVec(TARGET_MASS,threeVec(0.0,0.0,0.0));

    /* put them into the center of mass frame */
    Boost.set (beam + target);
    fourVec CMbeam = Boost * beam;
    fourVec CMtarget = Boost * target;
    double CMenergy = (CMbeam + CMtarget).t();

    /* generate the resonance and isobar */

    /* in case it was not set */
    if(masshigh == 0.)
      masshigh = CMenergy - Mass(Baryon);
    if (masshigh < cpcmcpThreshold) {
      cerr << "Meson high mass below 3 particle threshold" << endl;
      exit(1);
    }
    else if (masshigh > ( CMenergy - Mass(Baryon))) {
      masshigh =  CMenergy - Mass(Baryon);
    }

    do {
      resonance_mass = randm(masslow, masshigh);
    } while (resonance_mass < cpcmcpThreshold);
    isobar1_mass = randm(cpcmThreshold, (resonance_mass - Mass(Cplus2)));

    /* beam, resonance, t, cos(theta) */
    double beam_p      = CMmomentum (CMenergy, Mass(Beam), PROTON_MASS);
    double resonance_p = CMmomentum (CMenergy, resonance_mass, Mass(Baryon));
    double resonance_E = sqrt (pow (resonance_mass, 2.0) + pow (resonance_p, 2.0));
    double costheta;
    double t;

    /* use t distribution: generated t in the range (tMin:tMax) */
    tMin = tmin(pbeam.z,Mass(Beam),PROTON_MASS,resonance_mass,Mass(Baryon));
        
    if (Isotropic)
      costheta = randm(-1.0,1.0);
    else {
      do {
       	t = getT(tMin,slope); 
	costheta = 1 + (t -tMin)/(2 * resonance_p * beam_p);
      } while (fabs(costheta) >=  1.0);
    }

    if ((resonance_p < 0.0) || (costheta > 1) || (costheta < -1))  {
      cerr << "Error" << endl;
    }
    
    resonance.polar(resonance_p,acos (costheta), randm (-M_PI,M_PI));
    resonance.t(resonance_E);

    // recoil particle 
    recoil.set(sqrt(resonance.V().lenSq() +  pow (Mass(Baryon), 2.0)),zeroVec - resonance.V());

    // now do decay in resonance rest frame 
    double isobar1_p = CMmomentum (resonance_mass, isobar1_mass, Mass(Cplus2));
    isobar1.polar(isobar1_p, acos(randm (-0.999999, 0.999999)), randm(-M_PI, M_PI)); 
    isobar1.t(sqrt (isobar1.V().lenSq () + pow (isobar1_mass, 2.0)));
    cplus2.set(sqrt(isobar1.V().lenSq() + pow (Mass(Cplus2), 2.0)), zeroVec-isobar1.V());

    // now do decay in isobar1 rest frame:  Isobar -> c+_1 c+_2 
    double cplus1_p = CMmomentum (isobar1_mass, Mass(Cplus1),Mass(Cminus));
    cplus1.polar( cplus1_p,acos (randm (-0.999999, 0.999999)),randm (-M_PI, M_PI)); 
    cplus1.t(sqrt (cplus1.V().lenSq () + pow (Mass(Cplus1), 2.0)));
    cminus.set(sqrt (cplus1.V().lenSq () + pow (Mass(Cminus), 2.0)),zeroVec - cplus1.V());
    
    // compute lorentz factor
    LorentzFactor = FlatMass ? 1 : (isobar1_p * cplus1_p);
    if (lfevents-- > 0) {
      lfmax = LorentzFactor > lfmax ? LorentzFactor : lfmax;
    }
    else {
      //generate vertices 
      threeVec production = threeVec (vbeam.x, vbeam.y, vbeam.z);
      if (LorentzFactor > randm (0.0, lfmax)) {//if (LorentzFactor > randm (0.0, lfmax)) 


	if (Print) {
	  cerr << "\n\n*** New Event --- before boosts to the lab:\n";
	  cerr << "Resonance\n";
	  cerr << "  Resonance mass in lab: " << resonance_mass << "\n";
	  cerr << "  Resonance CMmomentum in lab: " << resonance_p << "\n";
	  cerr << "  Resonance in lab:\n "    ;
	  resonance.print();
	  cerr << "isobar1\n";
	  cerr << "  isobar1 mass in lab: " << isobar1_mass << "\n";
	  cerr << "  isobar1 momentum in lab: " << isobar1_p << "\n";
	  cerr << "  isobar1 in lab:\n    ";
	}

	//transform all 4-vectors back to lab frame 
        fourVec tmp;

	// boost from the isobar1:(c+_1 c-) rest frame to  resonance:(isobar1 c+_2) rest frame. 
        tmp.set(isobar1.t() , zeroVec-isobar1.V());
        Boost.set (tmp);
        cplus1 = Boost * cplus1;
        cminus = Boost * cminus;

        // boost from resonance:(isobar1 c+_2) rest frame to CM
        tmp.set(resonance.t(), zeroVec-resonance.V());
	Boost.set(tmp);

        isobar1 = Boost * isobar1;
        cplus1 = Boost * cplus1;
	cminus = Boost * cminus;
        cplus2 = Boost * cplus2;
	
        // boost from CM to the lab frame 
        Boost.set(CMtarget);
        resonance = Boost * resonance;
        recoil = Boost * recoil;
        isobar1 = Boost * isobar1;
        cplus1 = Boost * cplus1;
        cminus = Boost * cminus;
        cplus2 = Boost * cplus2;
	
	if (Print) {
	  cerr << "\n\n***  -----------------------\n";
	  cerr << "Beam:\n  ";
	  beam.print();
	  cerr << "Beam in CM:\n  ";
	  CMbeam.print();
	  cerr << "Target in CM:\n  " ;
	  CMtarget.print();
	  cerr << "Resonance\n";
	  cerr << "  Resonance mass in lab: " << resonance_mass << "\n";
	  cerr << "  Resonance CMmomentum in lab: " << resonance_p << "\n";
	  cerr << "  t: " << t << "\n";
	  cerr << "  Resonance in lab:\n "    ;
	  resonance.print();
	  cerr << "recoil: \n  ";
	  recoil.print();
	  cerr << "isobar1\n";
	  cerr << "  isobar1 mass in lab: " << isobar1_mass << "\n";
	  cerr << "  isobar1 momentum in lab: " << isobar1_p << "\n";
	  cerr << "  isobar1 in lab:\n    ";
	  isobar1.print();
	  cerr << "c+1 :\n  ";
	  cplus1.print();
	  cerr << "c+2:\n  ";
	  cplus2.print();
	  cerr << "c-:\n  ";
	  cminus.print();
	  cerr << "Lorentz factor: " << LorentzFactor << "\n";
	  cerr << "icount: " << icount << endl;
	}
	// calculate masses for dalitz plots
	if (dalitz) {
	  cout << resonance_mass << " " << isobar1_mass*isobar1_mass  << " ";
	  cout << ~(cplus2 + cminus + cplus1) << " ";
	  cout << pow (~(cplus2 + cplus1), 2.0) << " "
	       << pow (~(cplus2 + cminus), 2.0) << " "
	       << pow (~(cplus1 + cminus), 2.0) << " ";
	  cout << masslow << " " << masshigh << " ";
	  cout << (cplus2 + cminus + cplus1 + recoil).lenSq() << " ";
	  cout << (beam - cminus - cplus1 - cplus2).lenSq() << " ";
	  cout << endl;
	} 
	else if (debug) {
	  cout << "Z " << production.x() << " " << production.y() << " " << production.z() << endl;
	}
	else if (txt2part_style){
	  cout << "4" << endl;
	  cout << EbeamZ << " " << beam.t() << " " << -production.z()/SPEED_OF_LIGHT << endl; 
	  pParticle_txt2part(Baryon, production, recoil);
	  pParticle_txt2part(Cminus, production, cminus);
	  pParticle_txt2part(Cplus1, production, cplus1);
	  pParticle_txt2part(Cplus2, production, cplus2);
	} /* end of if(txt2part_style) */

	else if(gamp) { //if(gamp)
	  event evt;
	  particle gBeam(PDGtable.get(pid2name(Beam)),Q(Beam));
	  particle gTarget(PDGtable.get(pid2name(Target)),Q(Target));
	  particle gRecoil(PDGtable.get(pid2name(Baryon)),Q(Baryon));
	  particle gCminus(PDGtable.get(pid2name(Cminus)),Q(Cminus));
	  particle gCplus1(PDGtable.get(pid2name(Cplus1)),Q(Cplus1));
	  particle gCplus2(PDGtable.get(pid2name(Cplus2)),Q(Cplus2));

	  gBeam.set4P(beam);
	  gTarget.set4P(target);
	  gRecoil.set4P(recoil);
	  gCminus.set4P(cminus);
	  gCplus1.set4P(cplus1);
	  gCplus2.set4P(cplus2);

	  evt.beam(gBeam);
	  evt.target(gTarget);
	  evt.addfinal(gRecoil);
	  evt.addfinal(gCminus);
	  evt.addfinal(gCplus1);
	  evt.addfinal(gCplus2);
	  
	  cout << evt;
	} /* end of if(gamp) */

	else { /* write event */
	  if (printBeam)
	    pParticle(Beam,production,beam);
	  if (printBaryon) {
	    pParticle(Baryon,production,recoil);
	  }
	  pParticle(Cplus1,production,cminus);
	  pParticle(Cminus,production,cplus1);
	  pParticle(Cplus2,production,cplus2);
	}
	nevents++;
	if (verbose) {
	  if (!(nevents % 100)) 
	    cerr << nevents << "\r" << flush;
	}
	maxevents--;
      }
    }
  }

  if (verbose)
    cerr << nevents << " Events generated" << endl;
}

/*----------------End of charged1 charged2 neutral-----------------------------------*/

int comp_double (const void* a, const void* b)
{
  double* da = (double*) a;
  double* db = (double*) b;

  if(*da<*db){
    return -1;
  }
  else if(*da>*db){
    return 1;
  }
  else {
    return 0;
  }
}

double randm (double low, double high)
{
  return ((high - low) * drand48 () + low);
/*  drand48() 
    returns non-negative double-precision floating point values uniformly distributed between [0.0, 1.0). */
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

double CMmomentum (double cm_engy, double m1, double m2)
{
    double A = cm_engy*cm_engy;
    double B = (m1 + m2)*(m1 + m2);
    double C = (m1 - m2)*(m1-m2);
    double D = (A - B) * (A - C);
    return( ((D < 0 ) ? -sqrt(-D) : sqrt(D))/(2.0 * cm_engy));
}

void generateBeamMCinsideTarget(vector3_t *v,vector3_t *p)
{
  if (UseTarget) {
    distribute_vertex(v);
  }
  else {
    v->x = v->y = 0.0;
    v->z = Vertex_Z_Center;
  }

  p->x = p->y = 0.0; 
  p->z = GetBeamP();
}
void distribute_vertex(vector3_t *v)
{
 float r1,r2;

 r1=gaussian_gen();
 v->x=VERTEX_X_CENTER + r1*VERTEX_X_SIGMA;
 r2=gaussian_gen();
 v->y=VERTEX_Y_CENTER + r2*VERTEX_Y_SIGMA;
 v->z=randm(targetZ0,targetZ1);
}

float GetBeamP()
{
  if (UseRange)
    return(randm(range0,range1));
  else if (UseBrem)
    return(brem(brem0,brem1));
  else
    return(pbeamZ);
}

double brem(double brem0,double brem1)
{
  return(brem0 * exp(randm(0,1) * log(brem1/brem0)));
}

void pParticle_txt2part(Particle_t type,threeVec v,fourVec p)
{
  cout << (int)type << endl;;
  cout << p.t() << " " << p.x() << " " << p.y() << " " << p.z() << endl;
  cout << v.x() << " " << v.y() << " " << v.z() << endl;; 
}
void pParticleGamp(Particle_t type,fourVec p)
{
  cout << (int)type << " ";
  cout << (int) Q(type) << " ";
  cout << p.x() << " " << p.y() << " " << p.z() << " " << p.t() << endl;
}
void pParticle(Particle_t type,threeVec v,fourVec p)
{
  cout << (int) type << " ";
  cout << v.x() << " " << v.y() << " " << v.z() << " ";
  cout << p.t() << " " << p.x() << " " << p.y() << " " << p.z() << endl;
}

float wcm(float p,float m1, float m2)
{
  return(sqrt(p*p + m1*m1) + sqrt(p*p + m2 * m2));
}

float pprime(float w, float m, float m3)
{
  return( sqrt( pow( (w*w - m*m + m3*m3)/(2.0 * w),2) - m3*m3));
}

double getT(double tMin,double slope)
{
  double t;
  int icount = 0;
  do {
    if((icount++) > 100000) {
      cerr << "getT: exiting... too many counts\t tMin: " << tMin << "\tslope: " << slope  << endl;
      exit(0);
    }
    t = log(randm(0.,1.0))/slope;
  } while( t > tMin);
  return(t);
}

float tmin(float plab,float ma,float mb,float mc,float md)
{
  float w = sqrt(s(plab,ma,mb));
  float pcma = pprime(w,ma,mb);
  float pcmc = pprime(w,mc,md);
  float ea = e(pcma,ma);
  float ec = e(pcmc,mc);
  return(ma*ma + mc*mc - 2.0 * ea * ec + 2.0 * pcma * pcmc);
}

float s(float plab,float m1,float m2)
{
  return(m1*m1 + m2*m2 + 2.0 * sqrt(plab * plab + m1 * m1) * m2);
}
float e(float p,float m)
{
  return(sqrt(p * p + m * m));
}
float tmax(float plab,float ma,float mb,float mc,float md)
{
  float w = sqrt(s(plab,ma,mb));
  float pcma = pprime(w,ma,mb);
  float pcmc = pprime(w,mc,md);
  float ea = e(pcma,ma);
  float ec = e(pcmc,mc);
  return(ma*ma + mc*mc - 2.0 * ea * ec - 2.0 * pcma * pcmc);
}

int  Q(Particle_t  pid)
{
  int ret = 0;
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
  case AntiProton:
    ret = -1;
    break;
  case Gamma:
  case Neutron:
  case AntiNeutron:
  case KLong:
  case KShort:
  case Pi0:
  case Rho0:
  case Eta:
  case EtaPrime:
  case omega:
  case phiMeson:
    ret = 0;
    break;
  default:
    ret =-100;
    break;
  }
  return(ret);
}

double Mass(Particle_t  pid)
{
  double  ret = 0;
  switch (pid) {
  case phiMeson:
    ret = PHI_MASS;
    break;
  case Eta:
    ret = ETA_MASS;
    break;
  case PiPlus:
  case PiMinus:
    ret = PI_MASS;
    break;
  case Proton:
  case AntiProton:
    ret = PROTON_MASS;
    break;
  case Neutron:
  case AntiNeutron:
    ret = NEUTRON_MASS;
    break;

  case KPlus:
  case KMinus:
    ret = KCHARGED_MASS;
    break;

  case Positron:
  case Electron:
    ret = 0.00051;
    break;
  case Gamma:
    ret = 0.0;
    break;
  case KLong:
  case KShort:
    ret = KZERO_MASS;
    break;
  case Pi0:
    ret = PI0_MASS;
    break;
  case omega:
    ret = OMEGA_MASS;
    break;
    
  default:
    ret =0.0;
    break;
  }
  return(ret);
}

void PrintParticleID()
{
  cerr << " Unknown = 0" << endl;
  cerr << " Gamma = 1" << endl;
  cerr << " Positron = 2" << endl;
  cerr << " Electron = 3" << endl;
  cerr << " Neutrino = 4" << endl;
  cerr << " MuonPlus = 5" << endl;
  cerr << " MuonMinus = 6" << endl;
  cerr << " Pi0 = 7" << endl;
  cerr << " PiPlus = 8" << endl;
  cerr << " PiMinus = 9" << endl;
  cerr << " KLong = 10" << endl;
  cerr << " KPlus = 11" << endl;
  cerr << " KMinus = 12" << endl;
  cerr << " Neutron = 13" << endl;
  cerr << " Proton = 14" << endl;
  cerr << " AntiProton = 15" << endl;
  cerr << " KShort = 16" << endl;
  cerr << " Eta = 17" << endl;
  cerr << " Lambda = 18" << endl;
  cerr << " SigmaPlus = 19" << endl;
  cerr << " Sigma0 = 20" << endl;
  cerr << " SigmaMinus = 21" << endl;
  cerr << " Xi0 = 22" << endl;
  cerr << " XiMinus = 23" << endl;
  cerr << " OmegaMinus = 24" << endl;
  cerr << " AntiNeutron = 25" << endl;
  cerr << " AntiLambda = 26" << endl;
  cerr << " AntiSigmaMinus = 27" << endl;
  cerr << " AntiSigma0 = 28" << endl;
  cerr << " AntiSigmaPlus = 29" << endl;
  cerr << " AntiXi0 = 30" << endl;
  cerr << " AntiXiPlus = 31" << endl;
  cerr << " AntiOmegaPlus = 32" << endl;
  cerr << " Deuteron = 45" << endl;
  cerr << " Triton = 49" << endl;
  cerr << " Rho0 = 57" << endl;
  cerr << " RhoPlus = 58" << endl;
  cerr << " RhoMinus = 59" << endl;
  cerr << " omega = 60" << endl;
  cerr << " EtaPrime = 61" << endl;
  cerr << " phiMeson = 62" << endl;
}

string pid2name(Particle_t  type) 
{
  switch (type) {
  case PiMinus:
  case PiPlus:
    return "pi";
    break;
  case KMinus:
  case KPlus:
    return "K";
    break;
  case Pi0:
    return "pi0";
    break;
  case Eta:
    return "eta";
    break;
  case phiMeson:
    return "phi(1020)";
    break;
  case omega:
    return ("omega(782)");
    break;
  case Proton:
    return "p";
    break;
  case AntiProton:
    return("pbar");
    break;
  case Neutron:
    return "n";
    break;
  case Gamma:
    return ("gamma");
    break;
  case Electron:
  case Positron:
    return ("e");
    break;
  default:
    return "unknown";
    break;
  }
}

void MUsage(char *ProcessName)
{
  cerr << ProcessName << " generates phase space monte carlo events\n\n\n";
  cerr << "First you must choose a mode: -M# \n";
  cerr << "\t7:\t\tgamma p -> pi+ pi- pi+ n" << endl;
  cerr << "\nAfter choosing the appropriate mode, call mcgen again with the -h option, eg:";
  cerr << "\nmcgen -M# -h\nThis will document options appropriate to that particular mode\n\n"; 
}

void GeneralUsage()
{
  cerr << "\t-P#        : Set photon beam momentum to # (default: 5.7 GeV)" << endl;
  cerr << "\t-E#        : Set primary electron beam momentum to # (default: 5.7 GeV)" << endl;
  cerr << "\t-r#,#      : Generate beam momentum uniform over given range (default = 4.8,5.52)" << endl;
  cerr << "\t-b#,#      : Generate a brem beam over given range (default = 4.8,5.52)" << endl;
  cerr << "\t-z#,#      : Generate vertex z over given range (default = -112.0,-92.0)" << endl;
  cerr << "\t-j#        : Input Beam id (default = Gamma [1])" << endl;
  cerr << "\t-I         : Generate events isotropically" << endl;
  cerr << "\t-T         : Make events compatable with txt2part" << endl;
  cerr << "\t-G         : Make events compatable with gamp" << endl;
}

void UsageM7 (char *ProcessName)
{
  cerr << ProcessName << " generates 3 pion monte carlo events\n\n\n";
  cerr << "Useage: " << ProcessName << " -mmaxevents -llfevents -Lmass -Umass -D -p -ofile -h\n";
  GeneralUsage();
  cerr << "\t-mmaxevents: write maxevents, default 999999\n";
  cerr << "\t-llfevents : use lfevents to find max. lorentz factor, default 3000\n";
  cerr << "\t-Llowmass  : lower bound for X mass, default: m(pi+ + pi- + pi+) GeV\n";
  cerr << "\t-Ulowmass  : upper bound for X mass, default 0 GeV\n";
  cerr << "\t-t         : t slope, default 1.7\n";
  cerr << "\t-e         : use 1/E factor in LIPS factor\n";
  cerr << "\t-F         : LorentzFactor will be set to 1\n";
  cerr << "\t-S         : Set a seed for srand48(seed)\n";
  cerr << "\t-B         : Print baryon, default (Beam, pi+, pi-, pi+). \n";
  cerr << "\t-A         : print all particles ==> print decay particls as well.\n";
  cerr << "\t================================================================================\n";
  cerr << "\t-v         : verbose mode\n";
  cerr << "\t-d         : print dalitz plot masses (to stdout)\n";
  cerr << "\t-p         : print events\n";
  cerr << "\t-D         : debug mode\n";
  cerr << "\t-h         : print usage\n";
  cerr << "\t-H         : print Particle Id Table and Particle Date Table.\n";
  cerr << flush;
}

// not used //////////////////////////////////////////////
#if 0
float eprime(float theta,float Qsq,float E)
{
  return(Qsq/(4. *  E * sin(theta/2.0) * sin(theta/2.0)));
}

double Qsq(double E,double Ep,double theta)
{
  return(4.0 * E * Ep * sin(theta/2.0) * sin(theta/2.0));
}

double nQsq(double E,double Ep,double theta)
{
  double p = sqrt(E*E - ELECTRON_MASS * ELECTRON_MASS);
  double pp = sqrt(Ep*Ep - ELECTRON_MASS * ELECTRON_MASS);
  double ret = 2.0 * (ELECTRON_MASS * ELECTRON_MASS - E * Ep + p * pp * cos(theta));
  return(ret);
}
	 
double Wsq(double E,double Ep,double theta)
{
  return(2.0 * PROTON_MASS * (E - Ep) - 2.0 * E * Ep * (1.0 - cos(theta)) + PROTON_MASS * PROTON_MASS);
}

float theta(float QSQ,float W,float E) 
{
  double k;
  
  k = (2. * PROTON_MASS * QSQ)/((2.0 * PROTON_MASS * E + PROTON_MASS * PROTON_MASS - W * W - QSQ) * (4.0 * E));
  if (fabs(k) <= 1.0 && k > 0.0) {
    return(2.0 * asin(sqrt(k)));
  }
  else
    return(-1000.0);
}

void distribute_beam(vector4_t *beam)
{
 float r1,r2,r3,phi,theta,p,r;

 r1=gaussian_gen();
 beam->t=BEAM_EN_CENTER + r1*BEAM_EN_SIGMA;
 r2=gaussian_gen();
 phi=BEAM_PHI_CENTER + r2*BEAM_PHI_SIGMA;
 r3=gaussian_gen();
 theta=BEAM_THETA_CENTER + r3*BEAM_THETA_SIGMA;
 p=sqrt((beam->t*beam->t) - (PI_MASS*PI_MASS));
 r=p*sin(theta);
 beam->space.x=r*cos(phi);
 beam->space.y=r*sin(phi);
 beam->space.z=p*cos(theta);
}
#endif
/////////////////////////////////////////////////////////////////////
