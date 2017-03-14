#include <iostream>
#include <sstream>
#include "Calibration.h"

using namespace std;

//----------------------- Calibration -----------------
Calibration::Calibration(): 
  caltype(c_begin),nparFit(0), nparSave(0), relative(false), item(i_unknown) {}

Calibration::Calibration(calibration_t caltype_, item_t item_): 
  caltype(caltype_) , saveOffs(0), relative(false),
  item(item_), histfile(NULL), name(NULL) {
  
  /// find defaults
  int defindex = c_begin;
  while (defindex < c_end && gDefault[defindex].caltype != caltype) defindex++;
  
  if (gDefault[defindex].caltype == caltype) {
    nparFit   = gDefault[defindex].nparFit;
    nparSave  = gDefault[defindex].nparSave;
    saveOffs  = gDefault[defindex].saveOffs;
    relative  = (gDefault[defindex].relative != 0);

    if (item == i_unknown) // already defined by paramter else
      item      = gDefault[defindex].item;
    for (int i=0; i<6; i++) {
      defltpar[i]   = gDefault[defindex].defltpar[i];
      lowlimit[i]   = gDefault[defindex].lowlimit[i];
      upplimit[i]   = gDefault[defindex].upplimit[i];
    }
    name = new string(gDefault[defindex].name);
  }
  else 
    throw "no default parameter found in gDefault"; 
}

const char* Calibration::GetLatexFunction () {
  static char s[255];
  strcpy (s, "f(x)   =   ");
  switch(caltype) {
  case  c_timewalk: 
    strcat(s, "p_{0} + #frac{p_{1}}{z^{p_{2}}}      z = #frac{x}{x_{0}} < p_{3}"); 
    break;
  case  c_tdc:      
  case  c_tdcp:      
    strcat(s, "p_{0}   +   p_{1} x   +   p_{2} x^{2}");
    break;
  case  c_atten:
  case  c_veff:     
    strcat(s, "p_{0}   +   p_{1} x");
    break;
  case  c_gmean:    
    strcat(s, "p_{0} landau ( #mu=p_{1}, #sigma=p_{2} )  +  p_{3}  +  p_{4} x");
    break;
  case  c_mass:     
    strcat(s, "p_{0}   exp ( - #frac{1}{2}   [#frac{ x - m_{p} - p_{1} }{p_{2}}]^{2}  )");
    break;
  case  c_gauss:    
  case  c_p2pdelay:
    strcat(s, "p_{0}   exp ( - #frac{1}{2}   [#frac{ x - p_{1} }{p_{2}}]^{2}  )");
    break;
  default:
    strcat(s, " N/A ");
    break;
  }
  return s;
}

const char* Calibration::GetHistName (int index) {
  if (index==3) return "chisquare";

  switch (caltype) {
  case  c_tdc: 
  case  c_tdcp: 
    switch (index) {
    case 0: return "T0_TDC";
    case 1: return "T1";
    case 2: return "T2";
    default: return NULL;
    }

  case  c_timewalk: 
    switch (index) {
    case 0: return "WALK1";
    case 1: return "WALK2";
    case 2: return "WALK_A0";
    default: return NULL;
    }

  case  c_atten:    
  case  c_veff:     
    switch (index) {
    case 0: return "slope";
    case 1: return "constant";
    case 2: return "err_slope";
    default: return NULL;
    }

  case  c_gmean:
  case  c_mass:
  case  c_p2pdelay:
    switch (index) {
    case 0: return "mean";
    case 1: return "sigma";
    case 2: return "area";

/*    case 0: return "height";
    case 1: return "mean";
    case 2: return "sigma"; */
    default: return NULL;
    }
    
  default:          return NULL;
  }
 
}

bool Calibration::IsLogy() {
  if (caltype == c_mass) return true;
  return false;
}

bool Calibration::IsTwoDim () {
  if (caltype == c_atten || caltype == c_veff || caltype == c_timewalk) return true;
  return false;
}

bool Calibration::IsGraph() {
  if (caltype == c_tdc || caltype == c_tdcp ) return true;
  return false;
}

const char* Calibration::GetItem(int lr) {
  switch (item) {
  case i_both:
    if (caltype == c_atten) lr--;
    switch (lr) {
    case -1: return "value";
    case  0: return "left";
    case  1: return "right";
    case  2: return "left";
    case  3: return "right";
    default: throw "non legal item lr-code";
    }
  case i_unknown: return "unknown"; 
  case i_left:    return "left";
  case i_right:   return "right"; 
  case i_p2p:     return "paddle2paddle";
  case i_value:   return "value";
  default: throw "non legal item code";
  }
}

const char* Calibration::GetSubsystem (int ipar) {
  switch (caltype) {
  case  c_tdc: 
  case  c_tdcp: 
    switch (ipar) {
    case 0: return "T0_TDC";
    case 1: return "T1";
    case 2: return "T2";
    default: throw "Subsystem: invalid parameter index for tdc calibration";
    }
  case  c_timewalk: 
    switch (ipar) {
    case 0: return "WALK1";
    case 1: return "WALK2";
    case 2: return "WALK_A0";
    //case 3: return "WALK_A0";
    default: throw "Subsystem: invalid parameter index for timewalk calibration";
    }
  case  c_atten:    
    switch (ipar) {
    case 0: return "Yoffset";
    case 1: 
    case 2: return "atten_length";
    default: throw "Subsystem: invalid parameter index for atten calibration";
    }
  case  c_veff:     return "veff";  
  case  c_gmean:    return "NMIP_ADC"; 
  case  c_p2pdelay: return "delta_T";
  default:  throw "Subsystem: calibration is not creating values for database";
  }
 
}

const char* Calibration::GetUncertainty(int ipar) {
  static char s[80];
  if (caltype==c_atten && ipar==1) {
    strcpy (s, "atten_u"); 
  }
  else 
    sprintf (s, "%su", GetSubsystem(ipar));
  return s;
}

bool Calibration::IsRootHist() {
  if (!histfile) return true;
  int isuffix = histfile->length() - 5;
  if (histfile->substr(isuffix,5)==".root")     return true;
  return false;
}

const char* Calibration::GetFileName() {
  if (histfile) {
    return histfile->c_str();
  }
  static char s[80];
  sprintf (s, "root/%s.root", GetName());
  return s;
}

const char* Calibration::GetWindowName() {
  static char s[80];
  if (caltype==c_tdc || caltype==c_tdcp || caltype==c_timewalk) 
    sprintf (s, "%s %s", GetName(), GetItem());
  else
    sprintf (s, "%s", GetName());
  return s;
}

const char* Calibration::GetName() {
  return name->c_str();
}

int Calibration::Parameter4Display(int iwindow) {
  if (iwindow < 0 || iwindow >= 3) 
    throw "parameter windows must be in 0,1,2";
  int retval = -1;
  switch (caltype) {
  case c_gauss:
  case c_tdcp: 
  case c_tdc: 
    { 
      int p4dis[3] = { 0, 1, 2 };
      retval = p4dis[iwindow];
    }  
    break;
  case c_atten:
  case c_veff: 
    {
      int p4dis[3] = { 1, 0, 11 };
      retval = p4dis[iwindow];
    }
    break;
  case c_timewalk: 
    {
      int p4dis[3] = { 1, 2, 3 };
      retval = p4dis[iwindow];
    }
    break;
  case c_gmean:
  case c_p2pdelay:
  case c_mass:
    {
      int p4dis[3] = { 1, 2, 99 };
      retval = p4dis[iwindow];
    }
    break;
  default: 
    {
      ostringstream errmsg;
      errmsg << "no survey histograms defined for calibration " << (*this);
      throw errmsg.str().c_str();
    }
    break;
  }
  return retval;
}

ostream& operator<< (ostream& os, Calibration& cal) {
  os << "c_" << cal.GetName();
  return os;
}

double Calibration::GetDefaultPar(int ipar) {
  if (ipar >= nparFit || ipar >= 6 || ipar < 0) 
    throw "Calibration::GetDefaultPar: bad parameter";
  return defltpar[ipar];
}

double Calibration::GetUpperLimit(int ipar) {
  if (ipar >= nparFit || ipar >= 6 || ipar < 0) 
    throw "Calibration::GetUpperLimit: bad parameter";
  return upplimit[ipar];
}

double Calibration::GetLowerLimit(int ipar) {
  if (ipar >= nparFit || ipar >= 6 || ipar < 0) 
    throw "Calibration::GetLowerLimit: bad parameter";
  return lowlimit[ipar];
}

void Calibration::SetDefaultPar(TF1* fitfun) {
  if (!fitfun) throw "Calibration::SetDefaultPar: no function defined";
  for (int i=0; i<nparFit; i++) {
    fitfun->SetParameter(i, GetDefaultPar(i));
  }
}

void Calibration::SetFitLimits(TF1* fitfun) {
  if (!fitfun) throw "Calibration::SetFitLimits: no function defined";
  for (int i=0; i<nparFit; i++) {
    fitfun->SetParLimits(i, GetLowerLimit(i), GetUpperLimit(i));
  }
}

/// number of value tables to check in, tables for uncertainties not counted
int Calibration::MaxTableValues() {
  if (item!=i_both) return nparSave;
  int retval = nparSave * 2;   // i_both: same calib.const. for item left and right
  return (caltype==c_atten? retval-1 : retval );
}

int Calibration::GetTableIndex(int index) {
  if (item!=i_both)     return saveOffs+index;
  if (caltype==c_atten) return (index? 1 : 0);
  return saveOffs+index/2;
}

int Calibration::IsRight() {
  switch (item) {
  case i_left:    return 0;
  case i_right:   return 1; 
  default: throw "not defined whether left or right item";
  }
}
