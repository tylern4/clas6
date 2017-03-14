#ifndef G12ANAL_H_
#define G12ANAL_H_


// Event_t definition:


#define NMODES 58

typedef enum { DstUnknown,DstPPipPim,DstPipPipPim,DstPPip,DstPKp,DstPKpKm,DstKpKmPip,DstPPipPi0,DstPKpPi0,DstPPipPimPi0,
	       DstGammas,DstProtonProton,DstPipPi0,DstP,DstPip,DstPPim,DstPipPim,DstN,DstNPip,DstNPim,
	       DstNPipPip,DstNPipPim,DstNoProton,DstPKm,DstKpKm,DstPPPbar,DstPKpKmPi0,DstPKpKmEta,DstKpKp,DstPPipGam,DstPPimGam,
	       DstPPipPimGam,DstPipPimGam,DstPipPipPimGam,DstPim,DstPipPip,DstEpEm,DstEpPim,DstPipEm,DstEp,DstEm,
	       DstPKpKmGammas,DstPKpKmGamma,DstPKpGammas,DstPKmGammas,DstKpKmGammas,DstNPipPipPim,DstKp,DstKm,
	       Dst2Plus1Neg,Dst1Plus1Neg,DstKpPipPim,DstKmPipPim,DstPKmPip,DstPPipPimGamGam,DstKpKpPim,DstK,DstPKpKmPipPim, DstAll} Event_t;

// -------------- function prototypes ------------------------

void printModes();
int  isEvent(Event_t eventType,clasEvent &evt);
char *ModeName(Event_t mode);
char *runPeriod(runPeriod_t run);
void padstring(char *s,int len);
unsigned int modes(clasEvent &evt);

double MMsq(Event_t eventType,clasEvent &evt);
double MMsqPipPipPim(clasEvent &evt);
double MMsqPKpKm(clasEvent &evt);
double MMsqPPipPim(clasEvent &evt);
double MMsqKpKm(clasEvent &evt);
double MMsqKpPipPim(clasEvent &evt);
double MMsqKpKmPip(clasEvent &evt);
double MMsqPKpKmPipPim(clasEvent &evt);

// ---------------------------------------------------------

// --------- code ------------------------------------------

void printModes() 
{
  int max = (int) DstAll;
  cerr << "\n\nModes" << endl;
  for (int i = 0; i <= max; ++i) {
    cerr << i << "\t" << ModeName((Event_t) i) << endl;
  }

}

double MMsq(Event_t eventType,clasEvent &evt)
{
  double ret = 0.0;

  switch (eventType) {
  case DstPipPipPim:
    ret = MMsqPipPipPim(evt) - PROTON_MASS * PROTON_MASS;
    break;
  case DstPKpKm:
    ret = MMsqPKpKm(evt);
    break;
  case DstPPipPim:
    ret = MMsqPPipPim(evt);
    break;
  case DstKpKm:
    ret = MMsqKpKm(evt);
    break;
  case DstKpPipPim:
    ret = MMsqKpPipPim(evt);
    break;
  case DstKpKmPip:
    ret = MMsqKpKmPip(evt);
    break;
  }
  return(ret);
}

double MMsqKpKmPip(clasEvent &evt)
{
  int npip = evt.N(PiPlus);
  int nKm = evt.N(KMinus);
  int nKp = evt.N(KPlus);
  double ret = 0.0;

  if (npip && nKm && nKp) {
    fourVec Kp,pip,Km,beam,target;
    pip = evt.cp(PiPlus,1).p();
    Kp = evt.cp(KPlus,1).p();
    Km = evt.cp(KMinus,1).p();


    beam = evt.beam().get4P();
    target = evt.target().get4P();
    ret = (beam + target - Kp  - pip - Km).lenSq();
  }
  return(ret);
}


double MMsqKpPipPim(clasEvent &evt)
{
  int npip = evt.N(PiPlus);
  int npim = evt.N(PiMinus);
  int nKp = evt.N(KPlus);
  double ret = 0.0;

  if (npip > 1 && npim && nKp) {
    fourVec Kp,pip,pim,beam,target;
    pip = evt.cp(PiPlus,1).p();
    Kp = evt.cp(KPlus,1).p();
    pim = evt.cp(PiMinus,1).p();


    beam = evt.beam().get4P();
    target = evt.target().get4P();
    ret = (beam + target - Kp  - pip - pim).lenSq();
  }
  return(ret);
}
double MMsqPipPipPim(clasEvent &evt)
{
  int npip = evt.N(PiPlus);
  int npim = evt.N(PiMinus);
  double ret = 0.0;

  if (npip > 1 && npim) {
    fourVec pip1,pip2,pim,beam,target;
    pip1 = evt.cp(PiPlus,1).p();
    pip2 = evt.cp(PiPlus,2).p();
    pim = evt.cp(PiMinus,1).p();


    beam = evt.beam().get4P();
    target = evt.target().get4P();
    ret = (beam + target - pip1 - pip2 - pim).lenSq();
  }
  return(ret);
}
double MMsqPKpKm(clasEvent &evt)
{
  double ret = -1000.0;
  if (isEvent(DstPKpKm,evt)) {
    fourVec p,kp,km,beam,target;
    kp = evt.cp(KPlus,1).p();
    km = evt.cp(KMinus,1).p();
    p = evt.cp(Proton,1).p();


    beam = evt.beam().get4P();
    target = evt.target().get4P();
    ret = (beam + target - p - kp - km).lenSq();
  }
  return(ret);
}
double MMsqPPipPim(clasEvent &evt)
{
  double ret = -1000.0;
  if (isEvent(DstPPipPim,evt)) {
    fourVec p,pip,pim,beam,target;
    pip = evt.cp(PiPlus,1).p();
    pim = evt.cp(PiMinus,1).p();
    p = evt.cp(Proton,1).p();


    beam = evt.beam().get4P();
    target = evt.target().get4P();
    ret = (beam + target - p - pip - pim).lenSq();
  }
  return(ret);
}

double MMsqKpKm(clasEvent &evt)
{
  double ret = -1000.0;
  if (isEvent(DstKpKm,evt)) {
    fourVec p,kp,km,beam,target;
    kp = evt.cp(KPlus,1).p();
    km = evt.cp(KMinus,1).p();


    beam = evt.beam().get4P();
    target = evt.target().get4P();
    ret = (beam + target - kp - km).lenSq();
  }
  return(ret);
}
double MMsqPKpKmPipPim(clasEvent &evt)
{
	double ret = -1000.0;
	if (isEvent(DstPKpKmPipPim,evt)) {
		fourVec p,kp,km,pip,pim,beam,target;
		kp = evt.cp(KPlus,1).p();
		km = evt.cp(KMinus,1).p();
		p = evt.cp(Proton,1).p();
		pip = evt.cp(PiPlus,1).p();
		pim = evt.cp(PiMinus,1).p();
		
		
		beam = evt.beam().get4P();
		target = evt.target().get4P();
		ret = (beam + target - p - kp - km - pip - pim).lenSq();
	}
	return(ret);
}


int  isEvent(Event_t eventType,clasEvent &evt)
{
  int npip = evt.N(PiPlus),
    nprot = evt.N(Proton),
    nkp = evt.N(KPlus), 
    nkm = evt.N(KMinus),
    npim = evt.N(PiMinus),
    npi0 = evt.N(Pi0),
    neta = evt.N(Eta),
    ngamma = evt.N(Gamma),
    nneut = evt.N(Neutron),
    npbar = evt.N(AntiProton),
    nem = evt.N(Electron),
    nep = evt.N(Positron);
  int nplus = npip + nprot + nkp + nep;
  int nneg = nkm + npim + nem + npbar;

  int nother = evt.N() - npip - nprot - nkp - nkm - npim - npi0 - ngamma - nneut - nep - nem;
  int ret = 0;
  int last = (int)DstAll;

    switch (eventType) {
    case DstEp:
      ret = (nep >= 1);
      break;
    case DstEm:
      ret = (nem >= 1);
      break;
    case DstP:
      ret = (nprot >= 1);
      break;
    case DstNoProton:
      ret = !nprot;
      break;
    case DstPPipPim:
      ret = (nprot) && (npip) && (npim);
      break; 
    case DstPPipPimGamGam:
    	ret = (nprot) && (npip) && (npim) && (ngamma > 1);
    	break;
    case DstPPip:
      ret = (nprot) && (npip);
      break;
    case DstPipPipPim:
      ret = (npip >= 2) && (npim);
      break;
    case DstKpPipPim:
      ret = nkp && npip  && npim;
      break; 
    case DstPKp:
      ret = (nprot) && (nkp);
      break; 
    case DstPKpKm:
      ret = (nprot) && (nkp) && (nkm);
      break;
    case DstKpKmPip:
      ret = (npip) && (nkp) && (nkm);
      break;
    case DstPPipPi0:
      ret = (nprot) && (npip) && (npi0);
      break; 
    case DstPKpPi0:
      ret = (nprot ) && (nkp ) && (npi0 );
      break;
    case DstPPipPimPi0:
      ret = (nprot ) && (npip ) && (npi0 ) && (npim ); 
      break;
    case DstGammas:
      ret = (ngamma > 1);
      break; 
    case DstProtonProton:
      ret = (nprot >= 2);
      break;
    case DstPipPi0:
      ret = npip && npi0;
      break;
    case DstPip:
      ret = npip;
      break;
    case DstPim:
      ret = npim;
      break;
    case DstKp:
      ret = nkp;
      break;
    case DstKm:
      ret = nkm;
      break;
    case DstPipPip:
      ret = npip >= 2;
      break;
    case DstPPim:
      ret = nprot && npim;
      break;
    case DstPipPim:
      ret = npip && npim;
      break;
    case DstPPPbar:
      ret = nprot >= 2 && npbar;
      break;
    case DstUnknown:
        ret = 0;
      for (int j = 1; j < last; ++j) {
	ret = isEvent((Event_t) j,evt) ? 1 : ret;
      }
      ret = !ret;
      break;
    case DstN:
      ret = !nprot && nneut;
      break;
    case DstNPip:
      ret = !nprot  && nneut && npip;
      break;
     
    case DstNPim:
      ret = !nprot && nneut && npim;
      break;
      
    case DstNPipPip:
      ret = !nprot && nneut && (npip > 1);
      break;

    case DstNPipPim:
      ret =  !nprot && nneut && npim && npip;
      break;

    case DstPKm:
      ret = nprot && nkm;
      break;

    case DstKpKm:
      ret = nkp && nkm;
      break;

    case DstPKpKmPi0:
      ret = nkp && nkm && npi0 && nprot;
      break;

    case DstPKpKmEta:
      ret = nkp && nkm && neta && nprot;
      break;

    case DstKpKp:
      ret = nkp >= 2;
      break;

    case DstPPipGam:
      ret = (nprot == 1) && (npip == 1) && (ngamma == 1);
      break;

    case DstPPimGam:
      ret = (nprot == 1) && (npim == 1) && (ngamma == 1);
      break;

    case DstPPipPimGam:
      ret = (nprot == 1) && (npip == 1) && (npim == 1) && (ngamma == 1);
      break;

    case DstPipPimGam:
      ret = (npip == 1) && (npim == 1) && (ngamma == 1);
      break;

	case DstKmPipPim:
      ret = nkm && npim && npip;
      break;

    case DstPKmPip:
      ret = nprot && nkm && npip;
      break;


	case DstPipPipPimGam:
      ret = (npip == 2) && (npim == 1) && (ngamma == 1);
      break;

    case DstEpEm:
      ret = (nep && nem);
      break;
    case DstEpPim:
      ret = nep && npim;
      break;
    case DstPipEm:
      ret = npip && nem;
      break;
    case DstPKpKmGammas:
      ret = nprot && nkp && nkm && (ngamma > 1);
      break;
    case DstPKpKmGamma:
      ret = nprot && nkp && nkm && ngamma;
      break;
    case DstPKpGammas:
      ret = nprot && nkp && (ngamma > 1);
      break;
    case DstPKmGammas: 
      ret = nprot && nkm && (ngamma > 1);
      break;
    case DstKpKmGammas:
      ret = nkp && nkm && (ngamma > 1);
      break;
    case DstNPipPipPim:
      ret = nneut && npip >= 2 && npim;
      break;
    case Dst2Plus1Neg:
      ret = nplus >= 2 && nneg;
      break;
    case Dst1Plus1Neg:
      ret = nplus && nneg;
      break;
    case DstKpKpPim:
          ret = nkp >= 2 && npim;
          break; 
    case DstK:
      ret = (nkp + nkm) > 0;
      break;
	case DstPKpKmPipPim:
	  ret = nprot && nkp && nkm && npip && npim; 
	  break;
    case DstAll:
      ret = 1;
      break;
	

    }


    return(ret);
}
char *runPeriod(runPeriod_t run)
{
  static char ret[10];
  switch(run) {

  case g1a:
    strcpy(ret,"g1a");
    break;
  case g1b:
    strcpy(ret,"g1b");
    break;

  case g1c:
    strcpy(ret,"g1c");
    break;
  case g2a:
    strcpy(ret,"g2a");
    break;

  case g2b:
    strcpy(ret,"g2b");
    break;
  case g6a:
    strcpy(ret,"g6a");
    break;

  case g6b:
    strcpy(ret,"g6b");
    break;
  case g6c:
    strcpy(ret,"g6c");
    break;

  case g7:
    strcpy(ret,"g7");
    break;

  case g8a:
    strcpy(ret,"g8a");
    break;
  case g10:
    strcpy(ret,"g10");
    break;
  case g11a:
    strcpy(ret,"g11a");
    break;
  case g12:
    strcpy(ret,"g12");
    break;
  default:
    strcpy(ret,"???");
    break;
  }
   padstring(ret,10);
  return(ret);
} 

char *ModeName(Event_t mode)
{
  static char ret[30];

  switch (mode) {
  case DstKpKmPip:
    strcpy(ret,"K+ K- pi+");
    break;
  case DstPPipPim:
    strcpy(ret,"p pi+ pi-");
    break;
  case DstPPipPimGamGam:
	  strcpy(ret,"p pi+ pi- gam gam");
	  break;
  case DstKpPipPim:
    strcpy(ret,"K+ pi+ pi-");
    break; 
  case DstKmPipPim:
    strcpy(ret,"K- pi+ pi-");
    break; 
  case DstPPip:
    strcpy(ret,"p pi+");
    break;
  case DstPipPipPim:
    strcpy(ret,"pi+ pi+ pi-");
    break; 
  case DstPKp:
    strcpy(ret,"p K+");
    break; 
  case DstPKm:
    strcpy(ret,"p K-");
    break; 
  case DstKpKm:
    strcpy(ret,"K+ K-");
    break; 
  case DstPKpKm:
    strcpy(ret,"p K+ K-");
    break;
  case DstPPipPi0:
    strcpy(ret,"p pi+ pi0");
    break; 
  case DstPKpPi0:
    strcpy(ret,"p K+ pi0");
    break;
  case DstPPipPimPi0:
    strcpy(ret,"p pi+ pi- pi0");
    break;
  case DstGammas:
    strcpy(ret,"gammas");
    break; 
  case DstProtonProton:
    strcpy(ret,"p p");
    break;
  case DstUnknown:
    strcpy(ret,"Unknown");
    break;
  case DstPipPi0:
    strcpy(ret,"pi+ pi0");
    break;
  case DstP:
    strcpy(ret,"p");
    break;
  case DstAll:
    strcpy(ret,"All");
    break;
  case DstPip:
    strcpy(ret,"pi+");
    break;
  case DstPim:
    strcpy(ret,"pi-");
    break;
  case DstPipPip:
    strcpy(ret,"pi+ pi+");
    break;
  case DstPPim:
    strcpy(ret,"p pi-");
    break;
  case DstPipPim:
    strcpy(ret,"pi+ pi-");
    break;
  case DstN:
    strcpy(ret,"n");
    break;
  case DstNPip:
    strcpy(ret,"n pi+");
    break;
  case DstNPim:
    strcpy(ret,"n pi-");
    break;
  case DstNPipPip:
    strcpy(ret,"n pi+ pi+");
    break;
  case DstNPipPim:
    strcpy(ret,"n pi+ pi-");
    break;
  case DstNoProton:
    strcpy(ret,"no proton");
    break;
  case DstPPPbar:
    strcpy(ret,"p p pbar");
    break;
  case DstPKpKmPi0:
    strcpy(ret,"p K+ K- pi0");
    break;

  case DstPKpKmEta:
    strcpy(ret,"p K+ K- eta");
    break;
  case DstKpKp:
    strcpy(ret,"K+ K+");
    break;
  case DstPPipGam:
    strcpy(ret,"p pi+ gam");
    break;
  case DstPPimGam:
    strcpy(ret,"p pi- gam");
    break;

  case DstPPipPimGam:
    strcpy(ret,"p pi+ pi- gam");
    break;

  case DstPipPimGam:
    strcpy(ret,"pi+ pi- gam");
    break;

 case DstPipPipPimGam:
    strcpy(ret,"pi+ pi+ pi- gam");
    break;
  case DstEpEm:
    strcpy(ret,"e+ e-");
    break;
  case DstEpPim:
    strcpy(ret,"e+ pi-");
      break;
  case DstPipEm:
    strcpy(ret,"pi+ e-");
    break;
  case DstEp:
    strcpy(ret,"e+");
    break;
  case DstEm:
    strcpy(ret,"e-");
    break;
  case DstPKpKmGammas:
    strcpy(ret,"p K+ K- gamma gamma");
    break;
  case DstPKpKmGamma:
    strcpy(ret,"p K+ K- gamma");
    break;
  case DstPKpGammas:
    strcpy(ret,"p K+ gamma gamma");
    break;
  case DstPKmGammas:
    strcpy(ret,"p K- gamma gamma");
    break; 
  case DstKpKmGammas:
    strcpy(ret,"K+ K- gamma gamma");
    break;
  case DstNPipPipPim:
    strcpy(ret,"n pi+ pi+ pi-");
    break;
  case DstKp:
    strcpy(ret,"K+");
    break;
  case DstKm:
    strcpy(ret,"K-");
    break;
  case Dst2Plus1Neg:
    strcpy(ret,"2+ 1-");
    break;
  case Dst1Plus1Neg:
    strcpy(ret,"1+ 1-");
    break; 
  case DstPKmPip:
    strcpy(ret,"p K- pi+");
    break;
  case DstKpKpPim:
     strcpy(ret,"K+ K+ pi-");
     break; 
  case DstK:
    strcpy(ret,"charged K");
    break;
  case DstPKpKmPipPim:
	strcpy(ret,"p K+ K- pi+ pi-");
	break;
  default:
    strcpy(ret,"illegal");
    break;
  }
  padstring(ret,30);
  return(ret);
}
void padstring(char *s,int len)
{
  int l = strlen(s);
  for (int i = l; i < len - 1; ++i) {
    *(s + i) = ' ';
  }
  *(s + len) = (char) NULL;
}

unsigned int modes(clasEvent &evt)
{
  unsigned int ret = 0;
  // leave out DstUnknown and DstAll
    int last = (int) DstAll;
    for (int i = 1; i < last ; ++i) {
      if (isEvent( (Event_t) i,evt)) {
	ret += (1 << (i-1));
      }
    }
    return(ret);
}
	



// ---------------------------------------------------------



#endif /*G12ANAL_H_*/
