/*
 * mcgen.cc
 */

static const char sccsid[] = "@(#)"__FILE__"\t1.69\tCreated 3/26/97 20:31:35, \tcompiled "__DATE__;

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <math.h>
#include <mcgen.h>
#include <particleType.h>

#define BUFSIZE 10000

void pParticle(Particle_t,ThreeVector,FourVector);


void Usage(char *);

static int runNo = 7000;
static float pbeamZ = 4.0;
static float z = 0.0;

int main (int argc, char *argv[])
{
  int mode = 0;
  int newargc = 0;

  char **newargv = (char **) malloc(argc * sizeof(char *));

  
  newargv[newargc++] = argv[0];	   
  

  srand48((long)time(NULL));

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
	case 'Z':
	  z = atof(++ptr);
	  break;
	case 'R':
	  runNo = atoi(++ptr);
	  break;
	case 'P':
	  /* beam momentum */
	  pbeamZ = atof(++ptr);
	  break;
	case 'M':
	  mode = atoi (++ptr);
	  usage = 0;
	  break;
	default:
	  newargv[newargc++] = argv[iarg];
	  break;
	}
      }
    }
    switch (mode) {
    case 1:
      pipipi (newargc, newargv);
      break;
    case 2:
      pippim(newargc, newargv);
      break;
    case 3:
      npip(newargc,newargv);
      break;
    case 4:
      ppi0(newargc,newargv);
      break;
    case 5:
      kpkspim(newargc,newargv);
      break;
    case 6:
      npip_gamma(newargc,newargv);
      break;
    default:
      MUsage (argv[0]);
      break;
    }
  }
}



// resonance -> pi+ pi-
// isobar1 -> piplus + piminus2

void pippim (int argc, char *argv[])
{
  FILE * itapeout;
  int Print = 0,
    maxevents = 9999999,
    nevents = 0,
    lfevents = 5000,
    dalitz = 0;
  double masslow = 2 * PI_MASS,
    masshigh = 3.0,
    r2 = 0.0,
    t_min = 0.0,
    t_max,
    slope = 3.0,
    LorentzFactor = 0,
    expt_min = exp (-slope * t_min),
    expt_max,
    lfmax = 0,
    resonance_mass;
  char *outfile = "default.dst";
  FourVector beam,
    target,
    resonance,
    recoil,
    pip,
    pim;

  vector3_t 
    vbeam,
    pbeam;

  LorentzTransform Boost;

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
	  masslow = atof (ptr);
	  break;
	case 'U':
	  ptr++;
	  masshigh = atof (ptr);
	  break;
	case 'p':
	  Print = 1;
	  break;
	case 'd':
	  dalitz = 1;
	  break;
	case 'o':
	  ptr++;
	  outfile = ptr;
	  cerr << "output file: " << outfile << "\n";
	  break;
	case 'M':
	  break;

	case 'h':
	  UsageM2 (argv[0]);
	  return;
	default:
	  fprintf (stderr, "unrecognized argument: %c\n", *ptr);
	  break;
	}
      }
    }
  

  /*
   *-- beam and target in lab frame
   */

  itapeout = fopen (outfile, "w");

  while (maxevents) {
    /*
     *-- use real beam distribution
     */
    generateBeamMCinsideTarget(&vbeam, &pbeam);

    /*
     *-- beam and target in lab frame
     */

    beam = FourVector (pbeam.x, pbeam.y, pbeam.z, sqrt(pow(pbeam.x,2.0) + pow(pbeam.y,2.0) +
                                                       pow(pbeam.z,2.0) + pow (BEAM_MASS, 2.0)));
    target = FourVector (0.0, 0.0, 0.0, TARGET_MASS);

    /*
     *-- put them into the center of mass frame
     */
    Boost.Define (beam + target);
    FourVector CMbeam = Boost * beam;
    FourVector CMtarget = Boost * target;
    double CMenergy = (CMbeam + CMtarget).t;
   double pi_pi_threshold = 2 * PI_MASS;

    /* in case it was not set */
    if(masshigh == 0.)
      masshigh = CMenergy - PROTON_MASS;

    if (masshigh < pi_pi_threshold) {
      cerr << "pi pi high mass below pi pi threshold" << endl;
      exit(1);
    }

    if (masshigh > CMenergy - PROTON_MASS)
      masshigh = CMenergy - PROTON_MASS;
    if (masslow > CMenergy - PROTON_MASS) {
      cerr << "Not enough beam energy... exiting" << endl;
      exit(1);
    }

    t_max = pow (CMmomentum (2 * CMenergy, masslow, PROTON_MASS), 2.0);
    expt_max = exp (-slope * t_max);

    /*
     *-- generate the resonance
     */

    if (masshigh < pi_pi_threshold) {
      cerr << "pi pi high mass below pi pi_threshold" << endl;
      exit (1);
    }

    do {
      resonance_mass = randm (masslow, masshigh);
    } while (resonance_mass < pi_pi_threshold);

    r2 = randm (0.0, 1.0);

    double resonance_p = CMmomentum (CMenergy, resonance_mass, PROTON_MASS);
    double resonance_E = sqrt (pow (resonance_mass, 2.0) + pow (resonance_p, 2.0));
    double costheta;
    double expt;
    double t;
    do {
      expt = randm (expt_min, expt_max);
      t = -log (expt) / slope;
      costheta = (CMbeam.t * resonance_E
		  - 0.5 * (t + pow (PI_MASS, 2.0) + pow (resonance_mass, 2.0)))
	/ (~CMbeam.v * resonance_p);
    } while (fabs (costheta) > 1.0);
    PolarVector Presonance;
    Presonance.theta = acos (costheta);
    Presonance.phi = randm (-3.1415, 3.1415);
    Presonance.r = resonance_p;
    resonance.v = Presonance;
    resonance.t = resonance_E;

    /*
     *-- recoil particle
     */
    recoil.v = ThreeVector (0.0, 0.0, 0.0) - (resonance.v);
    recoil.t = sqrt (recoil.v.lensq () + pow (PROTON_MASS, 2.0));

    /*
     *  now do decay in resonance rest frame
     */
    double pip_p = CMmomentum (resonance_mass, PI_MASS, PI_MASS);
    PolarVector Ppip;
    Ppip.theta = acos (randm (-0.9999, 0.9999));
    Ppip.phi = randm (-M_PI,M_PI);
    Ppip.r = pip_p;
    pip.v = Ppip;
    pip.t = sqrt (pip.v.lensq () + pow (PI_MASS, 2.0));
    pim.v = ThreeVector (0.0, 0.0, 0.0) - pip.v;
    pim.t = sqrt (pim.v.lensq () + pow (PI_MASS, 2.0));

    /*
     *  compute lorentz factor
     */
    LorentzFactor = resonance_p * (pip_p);
    if (lfevents-- > 0)
      lfmax = LorentzFactor > lfmax ? LorentzFactor : lfmax;
    else {
      if (LorentzFactor > randm (0.0, lfmax)) {
	//	FourVector tmpvec;
	/*
	 *  transform all 4-vectors back to lab frame
	 */
	FourVector tmp;
	ThreeVector zeroVec = ThreeVector (0.0, 0.0, 0.0);

	// boost from pi_pi rest frame to CM
	tmp.v = zeroVec - resonance.v;
	tmp.t = resonance.t;
	Boost.Define (tmp);
	pip *= Boost;
	pim *= Boost;

	// boost from CM to target rest frame (lab)
	Boost.Define (CMtarget);
	resonance *= Boost;
	recoil *= Boost;
	pip *= Boost;
	pim *= Boost;

	/*
	 *  generate vertices
	 */
	// e852 values
	ThreeVector production = ThreeVector (vbeam.x, vbeam.y, vbeam.z);
	//	ThreeVector decay;

	if (Print) {
	  cerr << "\n\n*** New Event\n";
	  cerr << "Beam:\n  " << beam;
	  cerr << "Beam in CM:\n  " << CMbeam;
	  cerr << "Target in CM:\n  " << CMtarget;
	  cerr << "Resonance\n";
	  cerr << "  Resonance mass: " << resonance_mass << "\n";
	  cerr << "  Resonance CMmomentum: " << resonance_p << "\n";
	  cerr << "  expt: " << expt << "\n";
	  cerr << "  t: " << t << "\n";
	  cerr << "  Resonance:\n     " << resonance;
	  cerr << "recoil: \n  " << recoil;
	  cerr << "pip\n";
	  cerr << "  pip mass: " << PI_MASS << "\n";
	  cerr << "  pip momentum: " << pip_p << "\n";
	  cerr << "  pip:\n    " << pip;
	  cerr << "pim :\n  " << pim;
	  cerr << "vertices:\n";
	  cerr << "  prod: " << production;
	  cerr << "Lorentz factor: " << LorentzFactor << "\n";
	}
	// calculate masses for dalitz plots
	if (dalitz) {
	  cerr << pow (~(pip + pim), 2.0) << endl;
	}
	/*
	 *  write event
	 */
	/*	  pParticle(Electron,production,beam); */
	  pParticle(PiPlus,production,pip);
	  pParticle(PiMinus,production,pim);
	  /*	  pParticle(Proton,production,recoil); */
	maxevents--;
      }
    }
  }
}

// resonance -> isobar1 + piminus1
// isobar1 -> piplus + piminus2

void pipipi (int argc, char *argv[])
{
  FILE * itapeout;
  int Print = 0,
  maxevents = 9999999,
  nevents = 0,
  lfevents = 5000,
  dalitz = 0;
  double 
    masslow = 3 * PI_MASS,
    masshigh = 0.,
    t_max,
    slope = 10.0,
    expt_max,
    LorentzFactor = 0,
    lfmax = 0,
    resonance_mass,
    isobar1_mass;
  char *outfile = "default.dst";
  FourVector 
    beam,
    target,
    resonance,
    recoil,
    piminus1,
    piplus,
    piminus2,
    isobar1;
  LorentzTransform Boost;
  vector3_t vbeam,pbeam;

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
	  masslow = atof (ptr);
	  break;
	case 'U':
	  ptr++;
	  masshigh = atof (ptr);
	  break;
	case 'p':
	  Print = 1;
	  break;
	case 'd':
	  dalitz = 1;
	  break;
	case 'M':
	  break;
	case 'o':
	  ptr++;
	  outfile = ptr;
	  cerr << "output file: " << outfile << "\n";
	  break;
	case 'h':
	  UsageM1 (argv[0]);
	  return;
	default:
	  fprintf (stderr, "unrecognized argument: %c\n", *ptr);
	  break;
	}
      }
    }


  double pi_pi_pi_threshold = 3 * PI_MASS;
  double pi_pi_threshold = 2 * PI_MASS;
  if(masslow < pi_pi_pi_threshold)
    masslow = pi_pi_pi_threshold;

 
  itapeout = fopen (outfile, "w");

  while (maxevents) {
    /*
     *-- use real beam distribution
     */
    generateBeamMCinsideTarget(&vbeam, &pbeam);

    /*
     *-- beam and target in lab frame
     */

    beam = FourVector (pbeam.x, pbeam.y, pbeam.z, sqrt(pow(pbeam.x,2.0) + pow(pbeam.y,2.0) +
                                                       pow(pbeam.z,2.0) + pow (BEAM_MASS, 2.0)));
    target = FourVector (0.0, 0.0, 0.0, TARGET_MASS);

    /*
     *-- put them into the center of mass frame
     */
    Boost.Define (beam + target);
    FourVector CMbeam = Boost * beam;
    FourVector CMtarget = Boost * target;
    double CMenergy = (CMbeam + CMtarget).t;

    /*
     *-- generate the resonance and isobar
     */

    /* in case it was not set */
    if(masshigh == 0.)
      masshigh = CMenergy - PROTON_MASS;
    if (masshigh < pi_pi_pi_threshold) {
      cerr << "pi_pi_pi high mass below pi_pi_pi_threshold" << endl;
      exit(1);
    }

    do {
      resonance_mass = randm(masslow, masshigh);
    } while (resonance_mass < pi_pi_pi_threshold);
    isobar1_mass = randm(pi_pi_threshold, (resonance_mass - PI_MASS));

    double beam_p      = CMmomentum (CMenergy, BEAM_MASS, TARGET_MASS);
    double resonance_p = CMmomentum (CMenergy, resonance_mass, PROTON_MASS);
    double resonance_E = sqrt (pow (resonance_mass, 2.0) + pow (resonance_p, 2.0));
    double costheta;
    double t;

    /* use distribution t' * exp(-slope*t'), where t' = abs(t - tmin) and
       0 <= t' <= 4*pbeam*presonance in CM */
    t_max = 4. * beam_p * resonance_p;
    expt_max = exp(-1.)/slope;
    do {
      t = randm(0., t_max);
    } while( randm(0.,expt_max) > t*exp(-slope*t) );
    costheta = 1. - 2.*t/t_max;

    PolarVector Presonance;
    Presonance.theta = acos (costheta);
    Presonance.phi = randm (-M_PI, M_PI);
    Presonance.r = resonance_p;
    resonance.v = Presonance;
    resonance.t = resonance_E;

    /*
     *-- recoil particle
     */
    recoil.v = ThreeVector (0.0, 0.0, 0.0) - (resonance.v);
    recoil.t = sqrt (recoil.v.lensq () + pow (PROTON_MASS, 2.0));

    /*
     *  now do decay in resonance rest frame
     */
    double isobar1_p = CMmomentum (resonance_mass, isobar1_mass, PI_MASS);
    PolarVector Pisobar1;
    Pisobar1.theta = acos (randm (-0.999999, 0.999999));
    Pisobar1.phi = randm (-M_PI, M_PI);
    Pisobar1.r = isobar1_p;
    isobar1.v = Pisobar1;
    isobar1.t = sqrt (isobar1.v.lensq () + pow (isobar1_mass, 2.0));
    piminus1.v = ThreeVector (0.0, 0.0, 0.0) - isobar1.v;
    piminus1.t = sqrt (piminus1.v.lensq () + pow (PI_MASS, 2.0));

    /*
     *  now do decay in isobar1 rest frame
     */
  // pi pi case
    double piplus_p = CMmomentum (isobar1_mass, PI_MASS,PI_MASS);
    PolarVector Ppiplus;
    Ppiplus.theta = acos (randm (-0.999999, 0.999999));
    Ppiplus.phi = randm (-M_PI, M_PI);
    Ppiplus.r = piplus_p;
    piplus.v = Ppiplus;
    piplus.t = sqrt (piplus.v.lensq () + pow (PI_MASS, 2.0));
    piminus2.v = ThreeVector (0.0, 0.0, 0.0) - piplus.v;
    piminus2.t = sqrt (piminus2.v.lensq () + pow (PI_MASS, 2.0));


    /*
     *  compute lorentz factor
     */
    LorentzFactor = (resonance_p * isobar1_p * piplus_p);
    if (lfevents-- > 0)
      lfmax = LorentzFactor > lfmax ? LorentzFactor : lfmax;
    else {
      if (LorentzFactor > randm (0.0, lfmax)) {
   //     FourVector tmpvec;
        /*
         *  transform all 4-vectors back to lab frame
         */
        FourVector tmp;
        ThreeVector zeroVec = ThreeVector (0.0, 0.0, 0.0);


        // boost from pi_pi(rho) rest frame to pi_pi_pi(a2/M) rest frame
        tmp.v = zeroVec - isobar1.v;
        tmp.t = isobar1.t;
        Boost.Define (tmp);
        piplus = Boost * piplus;
        piminus2 = Boost * piminus2;

        // boost from pi_pi_pi rest frame to CM
        tmp.v = zeroVec - resonance.v;
        tmp.t = resonance.t;
        Boost.Define (tmp);
        isobar1 = Boost * isobar1;
        piplus = Boost * piplus;
        piminus2 = Boost * piminus2;
        piminus1 = Boost * piminus1;

        // boost from CM to target rest frame (lab)
        Boost.Define (CMtarget);
        resonance = Boost * resonance;
        recoil = Boost * recoil;
        isobar1 = Boost * isobar1;
        piplus = Boost * piplus;
        piminus2 = Boost * piminus2;
        piminus1 = Boost * piminus1;

        /*
         *  generate vertices
         */
        // e852 values
        ThreeVector production = ThreeVector (vbeam.x, vbeam.y, vbeam.z);
    //    ThreeVector decay;


	if (Print) {
	  cerr << "\n\n*** New Event\n";
	  cerr << "Beam:\n  " << beam;
	  cerr << "Beam in CM:\n  " << CMbeam;
	  cerr << "Target in CM:\n  " << CMtarget;
	  cerr << "Resonance\n";
	  cerr << "  Resonance mass: " << resonance_mass << "\n";
	  cerr << "  Resonance CMmomentum: " << resonance_p << "\n";
	  cerr << "  t: " << t << "\n";
	  cerr << "  Resonance:\n     " << resonance;
	  cerr << "recoil: \n  " << recoil;
	  cerr << "isobar1\n";
	  cerr << "  isobar1 mass: " << isobar1_mass << "\n";
	  cerr << "  isobar1 momentum: " << isobar1_p << "\n";
	  cerr << "  isobar1:\n    " << isobar1;
	  cerr << "pi+ :\n  " << piplus;
	  cerr << "pi-2:\n  " << piminus2;
	  cerr << "pi-1:\n  " << piminus1;
	  cerr << "vertices:\n";
	  cerr << "  prod: " << production;
	  cerr << "Lorentz factor: " << LorentzFactor << "\n";
	}
	// calculate masses for dalitz plots
	  if (dalitz) {
	    cerr << pow (~(piminus2 + piplus), 2.0) << " "
	      << pow (~(piminus2 + piminus1), 2.0) << " "
		<< pow (~(piplus + piminus1), 2.0) << endl;
	  }
	/*
	 *  write event
	 */

	

	  /*	  pParticle(PiMinus,production,beam); */
	  pParticle(PiMinus,production,piplus);
	  pParticle(PiPlus,production,piminus1);
	  pParticle(PiPlus,production,piminus2);
	  /*	  pParticle(Proton,production,recoil); */
	  maxevents--;
      }
    }
  }
}

/*----------------End of 3 pi-----------------------------------*/

// resonance -> pi+ pi-
// isobar1 -> piplus + piminus2

void npip (int argc, char *argv[])
{
  FILE * itapeout;
  int Print = 0,
    maxevents = 9999999,
    nevents = 0,
    lfevents = 5000,
    dalitz = 0;
  double masslow = 2 * PI_MASS,
    masshigh = 3.0,
    r2 = 0.0,
    t_min = 0.0,
    t_max,
    slope = 3.0,
    LorentzFactor = 0,
    expt_min = exp (-slope * t_min),
    expt_max,
    lfmax = 0,
    resonance_mass;
  char *outfile = "default.dst";
  FourVector beam,
    target,
    electron,
    resonance,
    neutron,
    pip;

  vector3_t 
    vbeam,
    pbeam;

  LorentzTransform Boost;

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
	masslow = atof (ptr);
	break;
      case 'U':
	ptr++;
	masshigh = atof (ptr);
	break;
      case 'p':
	Print = 1;
	break;
      case 'd':
	dalitz = 1;
	break;
      case 'o':
	ptr++;
	outfile = ptr;
	cerr << "output file: " << outfile << "\n";
	break;
      case 'M':
	break;

      case 'h':
	UsageM2 (argv[0]);
	return;
      default:
	fprintf (stderr, "unrecognized argument: %c\n", *ptr);
	break;
      }
    }
  }
  

  /*
   *-- beam and target in lab frame
   */

  itapeout = fopen (outfile, "w");

  while (maxevents) {
    /*
     *-- use real beam distribution
     */
    generateBeamMCinsideTarget(&vbeam, &pbeam);

    /*
     *-- beam and target in lab frame
     */

    beam = FourVector (pbeam.x, pbeam.y, pbeam.z, sqrt(pow(pbeam.x,2.0) + pow(pbeam.y,2.0) +
                                                       pow(pbeam.z,2.0) + pow (BEAM_MASS, 2.0)));
    target = FourVector (0.0, 0.0, 0.0, TARGET_MASS);

    /*
     *-- put them into the center of mass frame
     */
    Boost.Define (beam + target);
    FourVector CMbeam = Boost * beam;
    FourVector CMtarget = Boost * target;
    double CMenergy = (CMbeam + CMtarget).t;
    double n_pi_threshold = NEUTRON_MASS + PI_MASS;

    /* in case it was not set */
    if(masshigh == 0.)
      masshigh = CMenergy - PI_MASS;

    if (masshigh < n_pi_threshold) {
      cerr << "n pi high mass below pi pi threshold" << endl;
      exit(1);
    }
    if (masshigh > CMenergy - PI_MASS)
      masshigh = CMenergy - PI_MASS;
    if (masslow > CMenergy - PI_MASS) {
      cerr << "Not enough beam energy... exiting" << endl;
      exit(1);
    }

    t_max = pow (CMmomentum (2 * CMenergy, masslow, PROTON_MASS), 2.0);
    expt_max = exp (-slope * t_max);

    /*
     *-- generate the resonance
     */
    do {
      resonance_mass = randm (masslow, masshigh);
    } while (resonance_mass < n_pi_threshold);

    r2 = randm (0.0, 1.0);

    double resonance_p = CMmomentum (CMenergy, resonance_mass, PI_MASS);
    double resonance_E = sqrt (pow (resonance_mass, 2.0) + pow (resonance_p, 2.0));
    double costheta;
    double expt;
    double t;
    double tmin; 
    tmin = pow(resonance_mass,2.0) + pow(PROTON_MASS,2) - 2.0 * CMtarget.t * resonance_E + 2.0 * ~CMtarget.v * resonance_p;
    expt_min = exp (-slope * tmin);

    do {
      expt = randm (expt_min, expt_max);
      t = -log (expt) / slope;
      /* t -s really -t */
      costheta =- (CMtarget.t * resonance_E
		   - 0.5 * (t + pow (PROTON_MASS, 2.0) + pow (resonance_mass, 2.0)))
	/ (~CMtarget.v * resonance_p);
    } while (fabs (costheta) > 1.0);
    PolarVector Presonance;
    Presonance.theta = acos (costheta);
    Presonance.phi = randm (-M_PI, M_PI);
    Presonance.r = resonance_p;
    resonance.v = Presonance;
    resonance.t = resonance_E;

    /*
     *  now do decay in resonance rest frame
     */
    double pip_p = CMmomentum (resonance_mass, NEUTRON_MASS, PI_MASS);
    PolarVector Ppip;
    Ppip.theta = acos (randm (-0.9999, 0.9999));
    Ppip.phi = randm (-3.1415, 3.1415);
    Ppip.r = pip_p;
    pip.v = Ppip;
    pip.t = sqrt (pip.v.lensq () + pow (PI_MASS, 2.0));
    neutron.v = ThreeVector (0.0, 0.0, 0.0) - pip.v;
    neutron.t = sqrt (neutron.v.lensq () + pow (NEUTRON_MASS, 2.0));

    /*
     *  compute lorentz factor
     */
    LorentzFactor = resonance_p * (pip_p);
    if (lfevents-- > 0)
      lfmax = LorentzFactor > lfmax ? LorentzFactor : lfmax;
    else {
      if (LorentzFactor > randm (0.0, lfmax)) {
	//	FourVector tmpvec;
	/*
	 *  transform all 4-vectors back to lab frame
	 */
	FourVector tmp;
	ThreeVector zeroVec = ThreeVector (0.0, 0.0, 0.0);

	// boost from n_pi rest frame to CM
	tmp.v = zeroVec - resonance.v;
	tmp.t = resonance.t;
	Boost.Define (tmp);
	pip *= Boost;
	neutron *= Boost;

	// boost from CM to target rest frame (lab)
	Boost.Define (CMtarget);
	resonance *= Boost;
	pip *= Boost;
	neutron *= Boost;

	/*
	 *  generate vertices
	 */
	// e852 values
	ThreeVector production = ThreeVector (0.0, 0.0, 0.0);
	//	ThreeVector decay;
	electron = beam + target -neutron - pip;
	if (Print) {
	  cerr << "\n\n*** New Event\n";
	  cerr << "Beam:\n  " << beam;
	  cerr << "Beam in CM:\n  " << CMbeam;
	  cerr << "Target in CM:\n  " << CMtarget;
	  cerr << "Resonance\n";
	  cerr << "  Resonance mass: " << resonance_mass << "\n";
	  cerr << "  Resonance CMmomentum: " << resonance_p << "\n";
	  cerr << "  expt: " << expt << "\n";
	  cerr << "  t: " << t << "\n";
	  cerr << " Qsq: "  << -(beam - electron).lensq() << endl; 
	  cerr << " t: " << (target - neutron - pip).lensq() << endl;
	  cerr << "  Resonance:\n     " << resonance;
	  cerr << "pip\n";
	  cerr << "  pip mass: " << PI_MASS << "\n";
	  cerr << "  pip momentum: " << pip_p << "\n";
	  cerr << "  pip:\n    " << pip;
	  cerr << "  neutron :\n  " << neutron;
	  cerr << "  electron :\n  " << electron;
	  cerr << "vertices:\n";
	  cerr << "  prod: " << production;
	  cerr << "Lorentz factor: " << LorentzFactor << "\n";
	}
	// calculate masses for dalitz plots
	if (dalitz) {
	  cerr << pow (~(pip + neutron), 2.0) << endl;
	}
	/*
	 *  write event
	 */
	pParticle(Electron,production,electron);
	pParticle(PiPlus,production,pip);
	pParticle(Neutron,production,neutron);
	maxevents--;
      }
    }
  }
}



void npip_gamma (int argc, char *argv[])
{
  FILE * itapeout;
  int Print = 0,
    maxevents = 9999999,
    nevents = 0,
    lfevents = 5000,
    dalitz = 0;
  double mass,
    resonance_mass;
  char *outfile = "default.dst";
  FourVector beam,
    target,
    resonance,
    neutron,
    pip;

  vector3_t 
    vbeam,
    pbeam;

  LorentzTransform Boost;

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
	case 'p':
	  Print = 1;
	  break;
	case 'd':
	  dalitz = 1;
	  break;
	case 'o':
	  ptr++;
	  outfile = ptr;
	  cerr << "output file: " << outfile << "\n";
	  break;
	case 'M':
	  break;

	case 'h':
	  UsageM6 (argv[0]);
	  return;
	default:
	  fprintf (stderr, "unrecognized argument: %c\n", *ptr);
	  break;
	}
      }
    }
  

  /*
   *-- beam and target in lab frame
   */

  itapeout = fopen (outfile, "w");

  while (maxevents) {
    /*
     *-- use real beam distribution
     */
    generateBeamMCinsideTarget(&vbeam, &pbeam);

    /*
     *-- beam and target in lab frame
     */

    beam = FourVector (pbeam.x, pbeam.y, pbeam.z, sqrt(pow(pbeam.x,2.0) + pow(pbeam.y,2.0) +
                                                       pow(pbeam.z,2.0) + pow (BEAM_MASS, 2.0)));
    target = FourVector (0.0, 0.0, 0.0, TARGET_MASS);

    /*
     *-- put them into the center of mass frame
     */
    Boost.Define (beam + target);
    FourVector CMbeam = Boost * beam;
    FourVector CMtarget = Boost * target;
    double CMenergy = (CMbeam + CMtarget).t;
   double n_pi_threshold = NEUTRON_MASS + PI_MASS;

      mass = CMenergy;


    /*
     *-- generate the resonance
     */

      resonance_mass = mass;

      if (resonance_mass < NEUTRON_MASS + PI_MASS) {
	cerr << "Not enough CM energy...exiting." << endl;
	exit(1);
      }


    double resonance_p = 0.0;
    double resonance_E = resonance_mass;
    double costheta;
    double expt;
    double t;

    resonance.v = ThreeVector(0.0,0.0,0.0);
    resonance.t = resonance_mass;

    /*
     *  now do decay in resonance rest frame
     */
    double pip_p = CMmomentum (resonance_mass, NEUTRON_MASS, PI_MASS);
    PolarVector Ppip;
    Ppip.theta = acos (randm (-0.9999, 0.9999));
    Ppip.phi = randm (-3.1415, 3.1415);
    Ppip.r = pip_p;
    pip.v = Ppip;
    pip.t = sqrt (pip.v.lensq () + pow (PI_MASS, 2.0));
    neutron.v = ThreeVector (0.0, 0.0, 0.0) - pip.v;
    neutron.t = sqrt (neutron.v.lensq () + pow (NEUTRON_MASS, 2.0));

  

	//	FourVector tmpvec;
	/*
	 *  transform all 4-vectors back to lab frame
	 */
	FourVector tmp;
	ThreeVector zeroVec = ThreeVector (0.0, 0.0, 0.0);

	// boost from CM to target rest frame (lab)
	Boost.Define (CMtarget);
	resonance *= Boost;
	pip *= Boost;
	neutron *= Boost;

	/*
	 *  generate vertices
	 */
	ThreeVector production = ThreeVector (0.0, 0.0, 0.0);
	//	ThreeVector decay;
	if (Print) {
	  cerr << "\n\n*** New Event\n";
	  cerr << "Beam:\n  " << beam;
	  cerr << "Beam in CM:\n  " << CMbeam;
	  cerr << "Target in CM:\n  " << CMtarget;
	  cerr << "Resonance\n";
	  cerr << "  Resonance mass: " << resonance_mass << "\n";
	  cerr << "  Resonance CMmomentum: " << resonance_p << "\n";
	  cerr << "  Resonance:\n     " << resonance;
	  cerr << "  pip\n";
	  cerr << "  pip mass: " << PI_MASS << "\n";
	  cerr << "  pip momentum: " << pip_p << "\n";
	  cerr << "  pip:\n    " << pip;
	  cerr << "  neutron :\n  " << neutron;
	  cerr << "vertices:\n";
	  cerr << "  prod: " << production;
	}

	/*
	 *  write event
	 */

	  pParticle(PiPlus,production,pip);
	  pParticle(Neutron,production,neutron);
	maxevents--;
  }
}







// resonance -> pi+ pi-
// isobar1 -> piplus + piminus2

void ppi0 (int argc, char *argv[])
{
  FILE * itapeout;
  int Print = 0,
    maxevents = 9999999,
    nevents = 0,
    lfevents = 5000,
    dalitz = 0;
  double masslow = 2 * PI_MASS,
    masshigh = 3.0,
    r2 = 0.0,
    t_min = 0.0,
    t_max,
    slope = 3.0,
    LorentzFactor = 0,
    expt_min = exp (-slope * t_min),
    expt_max,
    lfmax = 0,
    resonance_mass;
  char *outfile = "default.dst";
  FourVector beam,
    target,
    electron,
    resonance,
    proton,
    pi0;

  vector3_t 
    vbeam,
    pbeam;

  LorentzTransform Boost;

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
	  masslow = atof (ptr);
	  break;
	case 'U':
	  ptr++;
	  masshigh = atof (ptr);
	  break;
	case 'p':
	  Print = 1;
	  break;
	case 'd':
	  dalitz = 1;
	  break;
	case 'o':
	  ptr++;
	  outfile = ptr;
	  cerr << "output file: " << outfile << "\n";
	  break;
	case 'M':
	  break;

	case 'h':
	  UsageM2 (argv[0]);
	  return;
	default:
	  fprintf (stderr, "unrecognized argument: %c\n", *ptr);
	  break;
	}
      }
    }
  

  /*
   *-- beam and target in lab frame
   */

  itapeout = fopen (outfile, "w");

  while (maxevents) {
    /*
     *-- use real beam distribution
     */
    generateBeamMCinsideTarget(&vbeam, &pbeam);

    /*
     *-- beam and target in lab frame
     */

    beam = FourVector (pbeam.x, pbeam.y, pbeam.z, sqrt(pow(pbeam.x,2.0) + pow(pbeam.y,2.0) +
                                                       pow(pbeam.z,2.0) + pow (BEAM_MASS, 2.0)));
    target = FourVector (0.0, 0.0, 0.0, TARGET_MASS);

    /*
     *-- put them into the center of mass frame
     */
    Boost.Define (beam + target);
    FourVector CMbeam = Boost * beam;
    FourVector CMtarget = Boost * target;
    double CMenergy = (CMbeam + CMtarget).t;
   double n_pi_threshold = PROTON_MASS + PI_MASS;

    /* in case it was not set */
    if(masshigh == 0.)
      masshigh = CMenergy - PI_MASS;

    if (masshigh < n_pi_threshold) {
      cerr << "p pi high mass below p pi threshold" << endl;
      exit(1);
    }
   if (masshigh > CMenergy - PI_MASS)
      masshigh = CMenergy - PI_MASS;
    if (masslow > CMenergy - PI_MASS) {
      cerr << "Not enough beam energy... exiting" << endl;
      exit(1);
    }

    t_max = pow (CMmomentum (2 * CMenergy, masslow, PROTON_MASS), 2.0);
    expt_max = exp (-slope * t_max);

    /*
     *-- generate the resonance
     */
    do {
      resonance_mass = randm (masslow, masshigh);
    } while (resonance_mass < n_pi_threshold);

    r2 = randm (0.0, 1.0);

    double resonance_p = CMmomentum (CMenergy, resonance_mass, PI_MASS);
    double resonance_E = sqrt (pow (resonance_mass, 2.0) + pow (resonance_p, 2.0));
    double costheta;
    double expt;
    double t;
    double tmin; 
    tmin = pow(resonance_mass,2.0) + pow(PROTON_MASS,2) - 2.0 * CMtarget.t * resonance_E + 2.0 * ~CMtarget.v * resonance_p;
    expt_min = exp (-slope * tmin);

    do {
      expt = randm (expt_min, expt_max);
      t = -log (expt) / slope;
      /* t -s really -t */
      costheta =- (CMtarget.t * resonance_E
		  - 0.5 * (t + pow (PROTON_MASS, 2.0) + pow (resonance_mass, 2.0)))
	/ (~CMtarget.v * resonance_p);
    } while (fabs (costheta) > 1.0);
    PolarVector Presonance;
    Presonance.theta = acos (costheta);
    Presonance.phi = randm (-M_PI, M_PI);
    Presonance.r = resonance_p;
    resonance.v = Presonance;
    resonance.t = resonance_E;

    /*
     *  now do decay in resonance rest frame
     */
    double pi0_p = CMmomentum (resonance_mass, PROTON_MASS, PI_MASS);
    PolarVector Ppi0;
    Ppi0.theta = acos (randm (-0.9999, 0.9999));
    Ppi0.phi = randm (-3.1415, 3.1415);
    Ppi0.r = pi0_p;
    pi0.v = Ppi0;
    pi0.t = sqrt (pi0.v.lensq () + pow (PI_MASS, 2.0));
    proton.v = ThreeVector (0.0, 0.0, 0.0) - pi0.v;
    proton.t = sqrt (proton.v.lensq () + pow (PROTON_MASS, 2.0));

    /*
     *  compute lorentz factor
     */
    LorentzFactor = resonance_p * (pi0_p);
    if (lfevents-- > 0)
      lfmax = LorentzFactor > lfmax ? LorentzFactor : lfmax;
    else {
      if (LorentzFactor > randm (0.0, lfmax)) {
	//	FourVector tmpvec;
	/*
	 *  transform all 4-vectors back to lab frame
	 */
	FourVector tmp;
	ThreeVector zeroVec = ThreeVector (0.0, 0.0, 0.0);

	// boost from n_pi rest frame to CM
	tmp.v = zeroVec - resonance.v;
	tmp.t = resonance.t;
	Boost.Define (tmp);
	pi0 *= Boost;
	proton *= Boost;

	// boost from CM to target rest frame (lab)
	Boost.Define (CMtarget);
	resonance *= Boost;
	pi0 *= Boost;
	proton *= Boost;

	/*
	 *  generate vertices
	 */
	// e852 values
	ThreeVector production = ThreeVector (0.0, 0.0, 0.0);
	//	ThreeVector decay;
	  electron = beam + target -proton - pi0;
	if (Print) {
	  cerr << "\n\n*** New Event\n";
	  cerr << "Beam:\n  " << beam;
	  cerr << "Beam in CM:\n  " << CMbeam;
	  cerr << "Target in CM:\n  " << CMtarget;
	  cerr << "Resonance\n";
	  cerr << "  Resonance mass: " << resonance_mass << "\n";
	  cerr << "  Resonance CMmomentum: " << resonance_p << "\n";
	  cerr << "  expt: " << expt << "\n";
	  cerr << "  t: " << t << "\n";
	  cerr << " Qsq: "  << -(beam - electron).lensq() << endl; 
	  cerr << " t: " << (target - proton - pi0).lensq() << endl;
	  cerr << "  Resonance:\n     " << resonance;
	  cerr << "pi0\n";
	  cerr << "  pi0 mass: " << PI_MASS << "\n";
	  cerr << "  pi0 momentum: " << pi0_p << "\n";
	  cerr << "  pi0:\n    " << pi0;
	  cerr << "  proton :\n  " << proton;
	  cerr << "  electron :\n  " << electron;
	  cerr << "vertices:\n";
	  cerr << "  prod: " << production;
	  cerr << "Lorentz factor: " << LorentzFactor << "\n";
	}
	// calculate masses for dalitz plots
	if (dalitz) {
	  cerr << pow (~(pi0 + proton), 2.0) << endl;
	}
	/*
	 *  write event
	 */
	  pParticle(Electron,production,electron);
	  pParticle(Proton,production,proton);
	maxevents--;
      }
    }
  }
}



/* ---------------- K K pi -------------------------------------*/

/*  K+ Ks Pi- */
void kpkspim(int argc, char *argv[])
{
  FILE * itapeout;
  int Print = 0,
    maxevents = 9999999,
    nevents = 0,
    lfevents = 5000;
  double  masslow = KCHARGED_MASS + KZERO_MASS + PI_MASS,
    masshigh = 0,
    t_max,   t_min = 0.0,
    slope = 3.5,  
    expt_min = exp (-slope * t_min),
    expt_max,
    LorentzFactor = 0,
    lfmax = 0,
    resonance_mass,
    isobar1_mass;
  char *outfile = "default.dst";
  FourVector
    beam,
    target,
    resonance,
    recoil,
    kplus,
    kshort,
    piminus,
    pi1, pi2,
    isobar1;
  LorentzTransform Boost;
  vector3_t vbeam,pbeam;


  if (argc <= 1) {
    UsageM5(argv[0]);
    return;
  }
  else {
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
          masslow = atof (ptr);
          break;
        case 'U':
          ptr++;
          masshigh = atof (ptr);
          break;
         case 'p':
          Print = 1;
          break;
        case 'M':
          break;
        case 'o':
          ptr++;
          outfile = ptr;
          cerr << "output file: " << outfile << "\n";
          break;
        case 'h':
          UsageM5 (argv[0]);
          return;
        default:
          fprintf (stderr, "unrecognized argument: %c\n", *ptr);
          break;
        }
      }
    }
  }


  double k_k_pi_threshold = KCHARGED_MASS + KZERO_MASS + PI_MASS;
  double k_pi_threshold =  KZERO_MASS + PI_MASS;
  if(masslow < k_k_pi_threshold)
    masslow = k_k_pi_threshold;

  itapeout = fopen (outfile, "w");

  while (maxevents) {
    /*
     *-- use real beam distribution
     */
    generateBeamMCinsideTarget(&vbeam, &pbeam);


    /*
     *-- beam and target in lab frame
     */
/* beam mass changed to K- mass (kdb 11-18-96) */
    beam = FourVector (pbeam.x, pbeam.y, pbeam.z, sqrt(pow(pbeam.x,2.0) + pow(pbeam.y,2.0) + pow(pbeam.z,2.0) + pow(BEAM_MASS,2.0)));

    target = FourVector (0.0, 0.0, 0.0, TARGET_MASS);

    /*
     *-- put them into the center of mass frame
     */
    Boost.Define (beam + target);
    FourVector CMbeam = Boost * beam;
    FourVector CMtarget = Boost * target;
    double CMenergy = (CMbeam + CMtarget).t;

    /*
     *-- generate the resonance and isobar
     */

    /* in case it was not set */
    if(masshigh == 0.)
      masshigh = CMenergy - PROTON_MASS;
    if (masshigh < k_k_pi_threshold) {
      cerr << "k_k_pi high mass below k_k_pi_threshold" << endl;
      exit(1);
    }

    do {
      resonance_mass = randm(masslow, masshigh);
    } while (resonance_mass < k_k_pi_threshold);

    isobar1_mass = randm(k_pi_threshold, (resonance_mass - KCHARGED_MASS));

    double beam_p      = CMmomentum (CMenergy, BEAM_MASS, TARGET_MASS);
    double resonance_p = CMmomentum (CMenergy, resonance_mass, PROTON_MASS);
    double resonance_E = sqrt (pow (resonance_mass, 2.0) + pow (resonance_p, 2.0));
    double costheta;
    double expt;
    double t;  

    t_max = pow (CMmomentum (2 * CMenergy, masslow, PROTON_MASS), 2.0);   
    expt_max = exp (-slope * t_max);

    do {
      expt = randm (expt_min, expt_max);
      t = -log (expt) / slope;
      costheta = (CMbeam.t * resonance_E
		  - 0.5 * (t + pow (PI_MASS, 2.0) + pow (resonance_mass, 2.0)))
	/ (~CMbeam.v * resonance_p);
    } while (fabs (costheta) > 1.0);

    PolarVector Presonance;
    Presonance.theta = acos (costheta);
    Presonance.phi = randm (-M_PI, M_PI);
    Presonance.r = resonance_p;
    resonance.v = Presonance;
    resonance.t = resonance_E;

    /*
     *-- recoil particle
     */
    recoil.v = ThreeVector (0.0, 0.0, 0.0) - (resonance.v);
    recoil.t = sqrt (recoil.v.lensq () + pow (PROTON_MASS, 2.0));

    /*
     *  now do decay in resonance rest frame
     */
    double isobar1_p = CMmomentum (resonance_mass, isobar1_mass, KCHARGED_MASS); 
    PolarVector Pisobar1;
    Pisobar1.theta = acos (randm (-0.999999, 0.999999));
    Pisobar1.phi = randm (-M_PI, M_PI);
    Pisobar1.r = isobar1_p;
    isobar1.v = Pisobar1;
    isobar1.t = sqrt (isobar1.v.lensq () + pow (isobar1_mass, 2.0));    
    kplus.v = ThreeVector (0.0, 0.0, 0.0) - isobar1.v;
    kplus.t = sqrt (kplus.v.lensq () + pow (KCHARGED_MASS, 2.0));

    /*
     *  now do decay in isobar1 rest frame
     */
  // k pi case
    double kshort_p = CMmomentum (isobar1_mass, KZERO_MASS,PI_MASS);
    PolarVector Pkshort;
    Pkshort.theta = acos (randm (-0.999999, 0.999999));
    Pkshort.phi = randm (-M_PI, M_PI);
    Pkshort.r = kshort_p;
    kshort.v = Pkshort;
    kshort.t = sqrt (kshort.v.lensq () + pow (KCHARGED_MASS, 2.0));
    piminus.v = ThreeVector (0.0, 0.0, 0.0) - kshort.v;
    piminus.t = sqrt (piminus.v.lensq () + pow (PI0_MASS, 2.0));

         /* Kshort decay */


        double pi1_p = CMmomentum (KZERO_MASS, PI_MASS,PI_MASS);
        PolarVector Ppi1;
        Ppi1.theta = acos (randm (-1.0, 1.0));
        Ppi1.phi = randm (-M_PI, M_PI);
        Ppi1.r = pi1_p;
        pi1.v = Ppi1;
        pi1.t =  sqrt (pi1.v.lensq () + pow (PI_MASS, 2.0));
        pi2.v = ThreeVector (0.0, 0.0, 0.0) - pi1.v;
        pi2.t = pi1.t;


    /*
     *  compute lorentz factor
     */
    LorentzFactor = (resonance_p * isobar1_p * kshort_p);
    if (lfevents-- > 0)
      lfmax = LorentzFactor > lfmax ? LorentzFactor : lfmax;
    else {
      if (LorentzFactor > randm (0.0, lfmax)) {
        /*
         *  transform all 4-vectors back to lab frame
         */
        FourVector tmp;
        ThreeVector zeroVec = ThreeVector (0.0, 0.0, 0.0);
         /* Boost the pions  to Isobar  rest frame */
        tmp.v = zeroVec - kshort.v;
        tmp.t = kshort.t;
        Boost.Define (tmp);
        pi1 = Boost * pi1;
        pi2 = Boost * pi2;
 
        // boost to resonance rest frame

        tmp.v = zeroVec - isobar1.v;
        tmp.t = isobar1.t;
        Boost.Define (tmp);
        kshort = Boost * kshort;
        piminus = Boost * piminus;
        pi1 = Boost * pi1;
        pi2 = Boost * pi2;


        // boost from resonance rest frame to CM
        tmp.v = zeroVec - resonance.v;
        tmp.t = resonance.t;
        Boost.Define (tmp);
        isobar1 = Boost * isobar1;
        kshort = Boost * kshort;
        kplus = Boost * kplus;
        piminus = Boost * piminus;
        pi1 = Boost * pi1;
        pi2 = Boost * pi2;


        // boost from CM to target rest frame (lab)
        Boost.Define (CMtarget);
        resonance = Boost * resonance;
        recoil = Boost * recoil;
        isobar1 = Boost * isobar1;
        kshort = Boost * kshort;
        kplus = Boost * kplus;
        piminus = Boost * piminus;
        pi1 = Boost * pi1;
        pi2 = Boost * pi2;

        /*
         *  generate vertices
         */
        // e852 values
        ThreeVector production = ThreeVector (vbeam.x, vbeam.y, vbeam.z);
    //    ThreeVector decay;



        if (Print) {
          cout << "\n\n*** New Event\n";
          cout << "Beam:\n  " << beam;
          cout << "Beam in CM:\n  " << CMbeam;
          cout << "Target in CM:\n  " << CMtarget;
          cout << "Resonance\n";
          cout << "  Resonance mass: " << resonance_mass << "\n";
          cout << "  Resonance CMmomentum: " << resonance_p << "\n";          cout << "  t: " << t << "\n";
          cout << "  Resonance:\n     " << resonance;
          cout << "recoil: \n  " << recoil;
          cout << "isobar1\n";
          cout << "  isobar1 mass: " << isobar1_mass << "\n";
          cout << "  isobar1 momentum: " << isobar1_p << "\n";
          cout << "  isobar1:\n    " << isobar1;
          cout << "kshort :\n  " << kshort;
          cout << "pi-:\n  " << piminus;
          cout << "k+:\n  " << kplus;
          cout << "pi1:\n  " << pi1;
          cout << "pi2:\n  " << pi2;
          cout << "pi pi mass: " << ~(pi1 + pi2) << endl;
          cout << "vertices:\n";
          cout << "  prod: " << production;
          cout << "Lorentz factor: " << LorentzFactor << "\n";
        }

        /*
         *  write event
         */

	pParticle(PiMinus,production,pi1);
	pParticle(PiPlus,production,pi2);
	pParticle(KPlus,production,kplus);
	pParticle(PiMinus,production,piminus);
	  
        maxevents--;
      }
    }
  }
}



/*----------------End of specific final states -----------------------*/

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
}

double CMmomentum (double cm_engy, double m1, double m2)
{
    double A = cm_engy*cm_engy;
    double B = (m1 + m2)*(m1 + m2);
    double C = (m1 - m2)*(m1-m2);
    double D = (A - B) * (A - C);
    return( ((D < 0 ) ? -sqrt(-D) : sqrt(D))/(2.0 * cm_engy));
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

void distribute_vertex(vector3_t *v)
{
 float r1,r2;

 r1=gaussian_gen();
 v->x=VERTEX_X_CENTER + r1*VERTEX_X_SIGMA;
 r2=gaussian_gen();
 v->y=VERTEX_Y_CENTER + r2*VERTEX_Y_SIGMA;
 v->z=randm(162.8,192.8);
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

void Usage (char *ProcessName)
{
    cerr << "Useage: " << ProcessName << " -mmaxevents -llfevents -Lmass -Umass -D -p -ofile -h\n";
    cerr << "\t-mmaxevents: write maxevents, default 999999\n";
    cerr << "\t-llfevents : use lfevents to find max. lorentz factor, default 3000\n";
    cerr << "\t-Llowmass  : lower bound for X mass, default 1.5 GeV\n";
    cerr << "\t-Ulowmass  : upper bound for X mass, default 2.5 GeV\n";
    cerr << "\t-e         : use 1/E factor in LIPS factor\n";
    cerr << "\t-d         : print dalitz plot masses\n";
    cerr << "\t-p         : print events\n";
    cerr << "\t-ofile     : write to file\n";
    cerr << "\t-h         : print useage\n";
    cerr << flush;
}

void UsageM1 (char *ProcessName)
{
    cerr << ProcessName << " generates 3 pion monte carlo events\n\n\n";
    cerr << "Useage: " << ProcessName << " -mmaxevents -llfevents -Lmass -Umass -D -p -ofile -h\n";
    GeneralUsage();
    cerr << "\t-mmaxevents: write maxevents, default 999999\n";
    cerr << "\t-llfevents : use lfevents to find max. lorentz factor, default 3000\n";
    cerr << "\t-Llowmass  : lower bound for X mass, default 1.5 GeV\n";
    cerr << "\t-Ulowmass  : upper bound for X mass, default 2.5 GeV\n";
    cerr << "\t-e         : use 1/E factor in LIPS factor\n";
    cerr << "\t-d         : print dalitz plot masses (to stdout)\n";
    cerr << "\t-p         : print events\n";
    cerr << "\t-ofile     : write to file\n";
    cerr << "\t-h         : print useage\n";
    cerr << flush;
}


void GeneralUsage()
{
  cerr << "\t-P#\t\tSet beam momentum to # (default: 4.0 GeV)\n";
}

void MUsage (char *ProcessName)
{
  cerr << ProcessName << " generates phase space monte carlo events\n\n\n";
  cerr << "First you must choose a mode: -M# \n";
  cerr << "\t1:\t\tpi+ pi- pi-" << endl;
  cerr << "\t2:\t\tpi+ pi-" << endl;
  cerr << "\t3:\t\tn pi+" << endl;  
  cerr << "\t4:\t\tp pi0" << endl;
  cerr << "\nAfter choosing the appropriate mode, call mcgen again with the -h option, eg:";
  cerr << "\nmcgen -M# -h\nThis will document options appropriate to that particular mode\n"; 
}


void UsageM2 (char *ProcessName)
{
    cerr << ProcessName << " generates 2 pion monte carlo events\n\n\n";
    cerr << "Useage: " << ProcessName << " -mmaxevents -llfevents -Lmass -Umass -D -p -ofile -h\n";
    GeneralUsage();
    cerr << "\t-mmaxevents: write maxevents, default 999999\n";
    cerr << "\t-llfevents : use lfevents to find max. lorentz factor, default 3000\n";
    cerr << "\t-Llowmass  : lower bound for X mass, default 1.5 GeV\n";
    cerr << "\t-Ulowmass  : upper bound for X mass, default 2.5 GeV\n";
    cerr << "\t-e         : use 1/E factor in LIPS factor\n";
    cerr << "\t-d         : print dalitz plot masses (to stdout)\n";
    cerr << "\t-p         : print events\n";
    cerr << "\t-ofile     : write to file\n";
    cerr << "\t-h         : print useage\n";
    cerr << flush;
}

void UsageM3 (char *ProcessName)
{
    cerr << ProcessName << " generates neutron pi+  monte carlo events\n\n\n";
    cerr << "Useage: " << ProcessName << " -mmaxevents -llfevents -Lmass -Umass -D -p -ofile -h\n";
    GeneralUsage();
    cerr << "\t-mmaxevents: write maxevents, default 999999\n";
    cerr << "\t-llfevents : use lfevents to find max. lorentz factor, default 3000\n";
    cerr << "\t-Llowmass  : lower bound for X mass, default 1.5 GeV\n";
    cerr << "\t-Ulowmass  : upper bound for X mass, default 2.5 GeV\n";
    cerr << "\t-e         : use 1/E factor in LIPS factor\n";
    cerr << "\t-d         : print dalitz plot masses (to stdout)\n";
    cerr << "\t-p         : print events\n";
    cerr << "\t-ofile     : write to file\n";
    cerr << "\t-h         : print useage\n";
    cerr << flush;
}



void UsageM5 (char *ProcessName)
{
    cerr << ProcessName << " generates k+ ks pi- monte carlo events\n\n\n";
    cerr << "Useage: " << ProcessName << " -mmaxevents -llfevents -Lmass -Umass -D -p -ofile -h\n";
    GeneralUsage();
    cerr << "\t-mmaxevents: write maxevents, default 999999\n";
    cerr << "\t-llfevents : use lfevents to find max. lorentz factor, default 3000\n";
    cerr << "\t-Llowmass  : lower bound for X mass, default 1.5 GeV\n";
    cerr << "\t-Ulowmass  : upper bound for X mass, default 2.5 GeV\n";
    cerr << "\t-e         : use 1/E factor in LIPS factor\n";
    cerr << "\t-d         : print dalitz plot masses (to stdout)\n";
    cerr << "\t-p         : print events\n";
    cerr << "\t-ofile     : write to file\n";
    cerr << "\t-h         : print useage\n";
    cerr << flush;
}
void UsageM6(char *ProcessName)
{
    cerr << ProcessName << " generates gamma p -> neutron pi+  monte carlo events\n\n\n";
    cerr << "Useage: " << ProcessName << " -mmaxevents -llfevents -D -p -ofile -h\n";
    GeneralUsage();
    cerr << "\t-mmaxevents: write maxevents, default 999999\n";
    cerr << "\t-llfevents : use lfevents to find max. lorentz factor, default 3000\n";
    cerr << "\t-e         : use 1/E factor in LIPS factor\n";
    cerr << "\t-d         : print dalitz plot masses (to stdout)\n";
    cerr << "\t-p         : print events\n";
    cerr << "\t-ofile     : write to file\n";
    cerr << "\t-h         : print useage\n";
    cerr << flush;
}




void generateBeamMCinsideTarget(vector3_t *v,vector3_t *p)
{
  v->x = v->y = 0.0;
  v->z = z;
  p->x = p->y = 0.0;
  
  p->z = GetBeamP();
}


void pParticle(Particle_t type,ThreeVector v,FourVector p)
{

  cout << (int) type << " ";
  cout <<   v.X() << " " << v.Y() << " " << v.Z() << " ";
  cout << p.T() << " " << p.X() << " " << p.Y() << " " << p.Z() << endl;

}

float GetBeamP()
{
  return(pbeamZ);
}
