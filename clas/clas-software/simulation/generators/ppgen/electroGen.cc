/*
 * electroGen.cc
 */


#include <iostream>

#include <math.h>
#include <unistd.h> 
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>


#include <Vec.h>
#include <lorentz.h>
#include <pputil.h>


#define BUFSIZE 200000


#include <Vec.h>
#include <particleType.h>

#include <plib.h>
#include <electroEvent.h>

extern particleDataTable PDGtable;

double randm (double low, double high);
double CMmomentum2(double s, double m1_2, double m2_2);

void printUsage();
event cpcm(Particle_t Beam,fourVec beam,int chargeBeam,Particle_t Cplus,Particle_t Cminus,double masslow,double masshigh,double slope,double *LorentzFactor);
event cpcmn0N(Particle_t Beam,fourVec beam,int chargeBeam,Particle_t Cplus,Particle_t Cminus,Particle_t N0,int decay,double masslow,double masshigh,double slope,double *LorentzFactor);
double Mass(Particle_t  pid);
int Q(Particle_t pid);

double getT(double tMin,double slope);
string pid2name(Particle_t  type);
double tmin2(double plab,double ma2,double mb2,double mc2,double md2);
double pprime2(double s,double m_2,double m3_2);
double s2(double pbeam,double mbeam2,double mtarget2);

clasDetector clas;


/* ---------------- beam p -> c+ c- n0 N -------------------------------------*/

/*  example gamma* p -> K+ K- eta n */


int main(int argc,char **argv) 
{ 
  extern char *optarg;
  extern int optind;
  int c;
  char *word;  

  double phiLow = -M_PI;
  double phiHigh = M_PI;

  double massLow = 0.0;
  double massHigh = 2.5;
  double tSlope = 4.0;

 double Ep = 1.0;
  double E = 6.0;

  double lfmax = 0.0;
  int nlfmax = 10000;


  unsigned short seeds[3] = {1,2,3};
  int maxevents = 50000; 

  lowq2Tagger tagger;


  int mode = 0;

 while ( (c = getopt(argc,argv, "s:e:E:p:M:b:t:m:h")) != -1 ) {
     switch(c) { 
    case 'M':
      mode = atoi(optarg);
      break;

    case 's':
       word = strtok(optarg,",");
       if (word) {
	 seeds[0] = atoi(word);
	 word = strtok(NULL,",");
	 if (word) {
	   seeds[1] = atoi(word);
	   word = strtok(NULL," ");
	   if (word) {
	     seeds[2] = atoi(word);
	   }
	 }
       }
       break;

    case 'n':
      maxevents = atoi(optarg);
      break;
    case 'p':
      // set the phi range of the low q^2 tagger
      word = strtok(optarg,",");
      phiLow = atof(word);
      word = strtok(NULL," ");
      if (word)
	phiHigh = atof(word);
      tagger.setPhi(phiLow,phiHigh);
      break;
    case 'b':
      word = strtok(optarg,",");
      phiLow = atof(word);
      word = strtok(NULL," ");
      phiHigh = atof(word);
      clas.setBaryonPhi(phiLow,phiHigh);
      break;
    case 'm': // set mass limits
      word = strtok(optarg,",");
      massLow = atof(word);
      word = strtok(NULL," ");
      if (word)
	massHigh = atof(word);
      break;
    case 't':
      word = strtok(optarg,",");
      tSlope = atof(word);
      break;

   case 'e':
      Ep = atof(optarg);
      break;
    case 'E':
      E = atof(optarg);
      break;
 
    case 'h':
      printUsage();
      exit(0);
      break;
    }
  }

  
  seed48(seeds);

  PDGtable.initialize();
  //  PDGtable.print();


  // first, lfmax loop

  while (nlfmax--) {
    double LorentzFactor;

    double theta = 0.017;
    double phi = tagger.getPhi();
 
    fourVec beam;
    fourVec incident(sqrt(ELECTRON_MASS*ELECTRON_MASS + E*E),threeVec(0.0,0.0,E));
    fourVec recoil;

    threeVec tmp; 

    event evt; 
    particle incidentElectron(PDGtable.get(pid2name(Electron)),Q(Electron));
    particle scatteredElectron(PDGtable.get(pid2name(Electron)),Q(Electron));

   


    recoil.set(sqrt(ELECTRON_MASS*ELECTRON_MASS + Ep*Ep),tmp.polar(Ep,theta,phi));
    beam = incident - recoil;

    incidentElectron.set4P(incident);
    scatteredElectron.set4P(recoil);

    switch (mode) {
    case 7:
      evt = cpcmn0N(Gamma,beam,0,PiPlus,PiPlus,PiMinus,0,massLow,massHigh,tSlope,&LorentzFactor);
 
      break;
    case 24:
      evt = cpcm(Gamma,beam,0,KPlus,KMinus,massLow,massHigh,tSlope,&LorentzFactor);
      break;
    }
     lfmax = (LorentzFactor > lfmax) ? LorentzFactor : lfmax;
  }


  while (maxevents--) {

    double LorentzFactor;

    double theta = 0.017;
    double phi = tagger.getPhi();


    fourVec beam;
    fourVec incident(sqrt(ELECTRON_MASS*ELECTRON_MASS + E*E),threeVec(0.0,0.0,E));
    fourVec recoil;

    threeVec tmp; 

    event evt; 
    particle incidentElectron(PDGtable.get(pid2name(Electron)),Q(Electron));
    particle scatteredElectron(PDGtable.get(pid2name(Electron)),Q(Electron));


    recoil.set(sqrt(ELECTRON_MASS*ELECTRON_MASS + Ep*Ep),tmp.polar(Ep,theta,phi));
    beam = incident - recoil;

    incidentElectron.set4P(incident);
    scatteredElectron.set4P(recoil);
    
    switch (mode) {
    case 7:
      evt = cpcmn0N(Gamma,beam,0,PiPlus,PiPlus,PiMinus,0,massLow,massHigh,tSlope,&LorentzFactor);

      break;
    case 24:
      evt = cpcm(Gamma,beam,0,KPlus,KMinus,massLow,massHigh,tSlope,&LorentzFactor);
      break;
    }

    evt.addfinal(incidentElectron);
    evt.addfinal(scatteredElectron);
    if (LorentzFactor > randm(0.0,lfmax)) {
      cout << evt;
    }
    else {
      maxevents++;
    }
	
    

  }

}



event  cpcmn0N (Particle_t Beam,fourVec beam,int chargeBeam,Particle_t Cplus,Particle_t Cminus,Particle_t N0,int decay,double masslow,double masshigh,double slope,double *LorentzFactor)
{
  event evt;
  Particle_t Baryon;
  double 
    t_max,
    expt_max,
    lfmax = 0,
    resonance_mass,
    isobar1_mass;
  fourVec 
    target,
    resonance,
    recoil,
    cminus,
    cplus,
    n0,
    gamma1,
    gamma2,
    isobar1;
  lorentzTransform Boost;

  int Isotropic = 0;
  
  threeVec zeroVec = threeVec (0.0, 0.0, 0.0);
  float beamMass;
  float baryonmass;
  int printBaryon = 0;
  int printGamma = 0;
  int debug = 0;
  Particle_t Target = Proton;
  float tMin;

  double cpcmn0Threshold = Mass(Cplus) + Mass(Cminus) + Mass(N0);
  double cpcmThreshold = Mass(Cplus) + Mass(Cminus); 
  int qtot = chargeBeam + Q(Proton) - Q(Cplus) - Q(Cminus) -Q(N0);
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


  baryonmass = Mass(Baryon);

  if(masslow < cpcmn0Threshold)
    masslow = cpcmn0Threshold;

 
  target = fourVec (PROTON_MASS,threeVec(0.0,0.0,0.0));

  /*
   *-- put them into the center of mass frame
   */
  Boost.set (beam + target);
  fourVec CMbeam = Boost * beam;
  fourVec CMtarget = Boost * target;
  double CMenergy = (CMbeam + CMtarget).t();


    /*
     *-- generate the resonance and isobar
     */

    /* in case it was not set */
    if(masshigh == 0.)
      masshigh = CMenergy - Mass(Baryon);
    if (masshigh < cpcmn0Threshold) {
      cerr << "Meson high mass below 3 particle threshold" << endl;
      exit(1);
    }
    else if (masshigh > ( CMenergy - Mass(Baryon))) {
      masshigh =  CMenergy - Mass(Baryon);
    }

    do {
      resonance_mass = randm(masslow, masshigh);
    } while (resonance_mass < cpcmn0Threshold);
    isobar1_mass = randm(cpcmThreshold, (resonance_mass - Mass(N0)));

    double beam_p      = CMmomentum2(CMenergy*CMenergy, beam.lenSq(), PROTON_MASS * PROTON_MASS);
    double resonance_p = CMmomentum2(CMenergy*CMenergy, resonance_mass*resonance_mass, Mass(Baryon)*Mass(Baryon));
    double resonance_E = sqrt (pow (resonance_mass, 2.0) + pow (resonance_p, 2.0));
    double costheta;
    double t;
    double baryonPhi = clas.getBaryonPhi();
    double resonancePhi;

    /* use distribution t' * exp(-slope*t'), where t' = abs(t - tmin) and
       0 <= t' <= 4*pbeam*presonance in CM */
    tMin = tmin2(beam.r(),beam.lenSq(),PROTON_MASS*PROTON_MASS,resonance_mass*resonance_mass,Mass(Baryon)*Mass(Baryon));
    //    cerr << tMin << endl;
    t_max = 4. * beam_p * resonance_p;
    expt_max = exp(-1.)/slope;


    if (Isotropic)
      costheta = randm(-1.0,1.0);

    else {
      do {
	t = getT(tMin,slope);
	costheta = 1 + (t - tMin)/(2 * resonance_p * beam_p);
      } while (fabs(costheta) >=  1.0);

    }

    if ((resonance_p < 0.0) || (costheta > 1) || (costheta < -1))  {
      cerr << "Error" << endl;
    }

    resonancePhi = baryonPhi + M_PI;
    while(resonancePhi > M_PI)
      resonancePhi -= 2 * M_PI;

    resonance.polar(resonance_p,acos (costheta), resonancePhi);
    resonance.t(resonance_E);


    /*
     *-- recoil particle
     */
    recoil.set(sqrt(resonance.V().lenSq() +  pow (Mass(Baryon), 2.0)),zeroVec - resonance.V());

    /*
     *  now do decay in resonance rest frame
     */
    double isobar1_p = CMmomentum2(resonance_mass*resonance_mass, isobar1_mass*isobar1_mass, Mass(N0)*Mass(N0));
     
    isobar1.polar( isobar1_p, acos (randm (-0.999999, 0.999999)), randm (-M_PI, M_PI)); 
    //   isobar1.polar( isobar1_p, acos (1.0), 0.0);

    isobar1.t(sqrt (isobar1.V().lenSq () + pow (isobar1_mass, 2.0)));
    
    n0.set(sqrt(isobar1.V().lenSq() + pow (Mass(N0), 2.0)),zeroVec -isobar1.V());




    /*
     *  now do decay in isobar1 rest frame
     */
    // c+ c-
    double cplus_p = CMmomentum2(isobar1_mass*isobar1_mass, Mass(Cplus)*Mass(Cplus),Mass(Cminus)*Mass(Cminus));

    cplus.polar( cplus_p,acos (randm (-0.999999, 0.999999)),randm (-M_PI, M_PI)); 
    //   cplus.polar( cplus_p,acos (1.0),0.0);
    cplus.t(sqrt (cplus.V().lenSq () + pow (Mass(Cplus), 2.0)));
    cminus.set(sqrt (cplus.V().lenSq () + pow (Mass(Cminus), 2.0)),zeroVec - cplus.V());
    
    // neutral decay to 2gamma, in pi0 rest frame
    if (decay) {
      double gam1_p = CMmomentum2(Mass(N0)*Mass(N0), 0.0, 0.0);
      gamma1.polar(gam1_p,acos (randm (-0.999999, 0.999999)),randm (-M_PI, M_PI)); 
      gamma1.t(gam1_p);
      gamma2.set(gam1_p,threeVec (0.0, 0.0, 0.0) - gamma1.V());
      gamma2.t(gam1_p);
    }

    /*
     *  compute lorentz factor
     */
    *LorentzFactor = (isobar1_p * cplus_p);

    /* transform all 4-vectors back to lab frame */
    fourVec tmp;

    // boost gammas to neutral rest frame
    if (decay) {
      tmp.set(n0.t(),zeroVec - n0.V());
      Boost.set(tmp);
      gamma1 = Boost * gamma1;
      gamma2 = Boost * gamma2;
    }
    // boost from c+ c- rest frame to c+ c- n0(X) rest frame

    tmp.set(isobar1.t(),zeroVec - isobar1.V());
    Boost.set (tmp);
    cplus = Boost * cplus;
    cminus = Boost * cminus;

    // boost from 3 particle meson  rest frame to CM
    tmp.set(resonance.t(),zeroVec - resonance.V());
    Boost.set(tmp);

    isobar1 = Boost * isobar1;
    cplus = Boost * cplus;
    cminus = Boost * cminus;
    n0 = Boost * n0;
    if (decay) {
      gamma1 = Boost * gamma1;
      gamma2 = Boost * gamma2;
    }
 

    // boost from CM to target rest frame (lab)
    Boost.set (CMtarget);
    resonance = Boost * resonance;
    recoil = Boost * recoil;
    isobar1 = Boost * isobar1;
    cplus = Boost * cplus;
    cminus = Boost * cminus;
    n0 = Boost * n0;
    if (decay) {
      gamma1 = Boost * gamma1;
      gamma2 = Boost * gamma2;
    }
    {
      particle gBeam(PDGtable.get(pid2name(Beam)),chargeBeam);
      particle gTarget(PDGtable.get(pid2name(Target)),Q(Target));
      particle gRecoil(PDGtable.get(pid2name(Baryon)),Q(Baryon));
      particle gCminus(PDGtable.get(pid2name(Cminus)),Q(Cminus));
      particle gCplus(PDGtable.get(pid2name(Cplus)),Q(Cplus));
      particle gGamma1(PDGtable.get(pid2name(Gamma)),Q(Gamma));
      particle gGamma2(PDGtable.get(pid2name(Gamma)),Q(Gamma));
      particle gN0(PDGtable.get(pid2name(N0)),Q(N0));

      gBeam.set4P(beam);
      gTarget.set4P(target);
      gRecoil.set4P(recoil);
      gCminus.set4P(cminus);
      gCplus.set4P(cplus);
      if (decay) {
	gGamma1.set4P(gamma1);
	gGamma2.set4P(gamma2);
      }
      gN0.set4P(n0);

      evt.beam(gBeam);
      evt.target(gTarget);
      evt.addfinal(gRecoil);
      evt.addfinal(gCminus);
      evt.addfinal(gCplus);
      if (decay) {
	evt.addfinal(gGamma1);
	evt.addfinal(gGamma2);
      }
      evt.addfinal(gN0);
    }


  return(evt);
}


/*----------------End of charged1 charged2 neutral-----------------------------------*/


event  cpcm(Particle_t Beam,fourVec beam,int chargeBeam,Particle_t Cplus,Particle_t Cminus,double masslow,double masshigh,double slope,double *LorentzFactor)
{
  event evt;
  Particle_t Baryon;
  double 
    t_max,
    expt_max,
    lfmax = 0,
    resonance_mass,
    isobar1_mass;
  fourVec 
    target,
    resonance,
    recoil,
    cminus,
    cplus;
  lorentzTransform Boost;

  int Isotropic = 0;
  
  threeVec zeroVec = threeVec (0.0, 0.0, 0.0);
  float beamMass;
  float baryonmass;
  int printBaryon = 0;
  int printGamma = 0;
  int debug = 0;
  Particle_t Target = Proton;
  float tMin;

  double cpcmThreshold = Mass(Cplus) + Mass(Cminus); 
  int qtot = chargeBeam + Q(Proton) - Q(Cplus) - Q(Cminus);
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


  baryonmass = Mass(Baryon);

  if(masslow < cpcmThreshold)
    masslow = cpcmThreshold;

 
  target = fourVec (PROTON_MASS,threeVec(0.0,0.0,0.0));

  /*
   *-- put them into the center of mass frame
   */
  Boost.set (beam + target);
  fourVec CMbeam = Boost * beam;
  fourVec CMtarget = Boost * target;
  double CMenergy = (CMbeam + CMtarget).t();


    /*
     *-- generate the resonance
     */

    /* in case it was not set */
    if(masshigh == 0.)
      masshigh = CMenergy - Mass(Baryon);
    if (masshigh < cpcmThreshold) {
      cerr << "Meson high mass below 2 particle threshold" << endl;
      exit(1);
    }
    else if (masshigh > ( CMenergy - Mass(Baryon))) {
      masshigh =  CMenergy - Mass(Baryon);
    }

    do {
      resonance_mass = randm(masslow, masshigh);
    } while (resonance_mass < cpcmThreshold);

    double beam_p      = CMmomentum2(CMenergy*CMenergy, beam.lenSq(), PROTON_MASS * PROTON_MASS);
    double resonance_p = CMmomentum2(CMenergy*CMenergy, resonance_mass*resonance_mass, Mass(Baryon)*Mass(Baryon));
    double resonance_E = sqrt (pow (resonance_mass, 2.0) + pow (resonance_p, 2.0));
    double costheta;
    double t;
    double baryonPhi = clas.getBaryonPhi();
    double resonancePhi;

    /* use distribution t' * exp(-slope*t'), where t' = abs(t - tmin) and
       0 <= t' <= 4*pbeam*presonance in CM */
    tMin = tmin2(beam.r(),beam.lenSq(),PROTON_MASS*PROTON_MASS,resonance_mass*resonance_mass,Mass(Baryon)*Mass(Baryon));
    //    cerr << tMin << endl;
    t_max = 4. * beam_p * resonance_p;
    expt_max = exp(-1.)/slope;


    if (Isotropic)
      costheta = randm(-1.0,1.0);

    else {
      do {
	t = getT(tMin,slope);
	costheta = 1 + (t - tMin)/(2 * resonance_p * beam_p);
      } while (fabs(costheta) >=  1.0);

    }

    if ((resonance_p < 0.0) || (costheta > 1) || (costheta < -1))  {
      cerr << "Error" << endl;
    }

    resonancePhi = baryonPhi + M_PI;
    while(resonancePhi > M_PI)
      resonancePhi -= 2 * M_PI;

    resonance.polar(resonance_p,acos (costheta), resonancePhi);
    resonance.t(resonance_E);


    /*
     *-- recoil particle
     */
    recoil.set(sqrt(resonance.V().lenSq() +  pow (Mass(Baryon), 2.0)),zeroVec - resonance.V());

    /*
     *  now do decay in resonance rest frame
     */


    /*
     *  now do decay in resonance rest frame
     */
    // c+ c-
    double cplus_p = CMmomentum2(resonance_mass*resonance_mass, Mass(Cplus)*Mass(Cplus),Mass(Cminus)*Mass(Cminus));

    cplus.polar( cplus_p,acos (randm (-0.999999, 0.999999)),randm (-M_PI, M_PI)); 
    //   cplus.polar( cplus_p,acos (1.0),0.0);
    cplus.t(sqrt (cplus.V().lenSq () + pow (Mass(Cplus), 2.0)));
    cminus.set(sqrt (cplus.V().lenSq () + pow (Mass(Cminus), 2.0)),zeroVec - cplus.V());
    
    /*
     *  compute lorentz factor
     */
    *LorentzFactor = (cplus_p);

    /* transform all 4-vectors back to lab frame */
    fourVec tmp;

    // boost from c+ c- rest frame to CM frame

    tmp.set(resonance.t(),zeroVec - resonance.V());
    Boost.set (tmp);
    cplus = Boost * cplus;
    cminus = Boost * cminus;


    // boost from CM to target rest frame (lab)
    Boost.set (CMtarget);
    resonance = Boost * resonance;
    recoil = Boost * recoil;
    cplus = Boost * cplus;
    cminus = Boost * cminus;

    {
      particle gBeam(PDGtable.get(pid2name(Beam)),chargeBeam);
      particle gTarget(PDGtable.get(pid2name(Target)),Q(Target));
      particle gRecoil(PDGtable.get(pid2name(Baryon)),Q(Baryon));
      particle gCminus(PDGtable.get(pid2name(Cminus)),Q(Cminus));
      particle gCplus(PDGtable.get(pid2name(Cplus)),Q(Cplus));

      gBeam.set4P(beam);
      gTarget.set4P(target);
      gRecoil.set4P(recoil);
      gCminus.set4P(cminus);
      gCplus.set4P(cplus);

      evt.beam(gBeam);
      evt.target(gTarget);
      evt.addfinal(gRecoil);
      evt.addfinal(gCminus);
      evt.addfinal(gCplus);
    }


  return(evt);
}


/*----------------End of charged1 charged2 -----------------------------------*/


double randm (double low, double high)
{
  return ((high - low) * drand48 () + low);
}

double CMmomentum2 (double s, double m1_2, double m2_2)
{

  double x = s/8.0 + (m1_2*m1_2 + m2_2*m2_2)/(4.0 * s) - m1_2 * m2_2/(2.0 * s) - (m1_2 + m2_2)/2.0;
  
  return(pprime2(s,m1_2,m2_2));


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
    ret = PI_CHARGED_MASS;
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
    ret = KAON_CHARGED_MASS;
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
    ret = KAON_ZERO_MASS;
    break;
  case Pi0:
    ret = PI_ZERO_MASS;
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

double s2(double pbeam,double mbeam2,double mtarget2)
{
  {
    return(mbeam2+mtarget2+2.0*sqrt(pbeam*pbeam+mbeam2)*
sqrt(mtarget2));
  }
}


double tmin2(double plab,double ma2,double mb2,double mc2,double md2)
{
  double Ea2;
  double Ec2;
  double pcma;
  double pcmc;
  double w;
  double s;

 
  s = s2(plab,ma2,mb2);
  w = sqrt(s);
  if ((s > ma2 + mb2) && (s >= mc2 + md2)) {
    pcma = pprime2(s,ma2,mb2);
    pcmc = pprime2(s,mc2,md2);
    Ea2 = pcma*pcma + ma2;
    Ec2 = pcmc*pcmc + mc2;
    return(ma2+mc2-2.0*sqrt(Ea2)*sqrt(Ec2)+2.0*pcma*pcmc);
  }
  else 
    return(-1000.0);
}
    
    
double pprime2(double s,double m_2,double m3_2)
{
  {
    return(sqrt(0.25*pow(s-m_2+m3_2,2.0)/(s)-m3_2));
  }
}


// ------------------------- usage --------------------------

void printUsage()
{
  cerr << "Usage: \n\telectroGen [-M#] [-p#[,#]] [-b#[,#]] [-m#[,#]] [-t#] [-h]" << endl;
  cerr << "\t\t-M#\t\tGenerate # events" << endl;
  cerr << "\t\t-p#[,#]\t\tSet phi range of the tagger in radians (default = -Pi,Pi)" << endl;
  cerr << "\t\t-b#[,#]\t\tSet phi range of the scattered baryon in radians (default = -Pi,Pi)" << endl;
  cerr << "\t\t-m#[,#]\t\tSet mass range of the meson system (default = threshold,2.5)" << endl;
  cerr << "\t\t-t#\t\tSet the t-slope of the meson system (default = 4)" << endl;
  cerr << "\t\t-h\t\tPrint this message" << endl;
}
