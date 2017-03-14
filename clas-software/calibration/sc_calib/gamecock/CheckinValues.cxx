#include "Calibration.h"
#include "SConstants.h"
#include "CheckinValues.h"
#include "SlotAverage.h"

using namespace std;

extern Calibration* gCalib;
extern SConstants*  gConst[0];
extern int SC_Version_Flag;

double QUAD(double x);

//------------------- CheckinValues ---------------------------
CheckinValues::CheckinValues(int index, int iserr) {
  int ipar = gCalib->GetTableIndex(index);
  for (int i=0; i<N_CHANNEL; i++)
    v[i] = (iserr ? 
	    gConst[0]->GetParError(i, ipar) :
	    gConst[0]->GetParameter(i, ipar) );
}

/// constructor for NMIP_ADC (needs Y0 offsets, errors)
CheckinValues::CheckinValues(int nmip, double* y0, double* y0err) {
  int i0 = gCalib->GetSaveOffs();
  for (int i=0; i<N_CHANNEL; i++) {
    double gmn = gConst[0]->GetParameter(i, i0);
    double gerr = gConst[0]->GetParError(i, i0);
    double exp_y0h = exp(y0[i]/2.);
    switch (nmip) {
    case 0:     /// value left
      v[i]= gmn * exp_y0h;
      break;
    case 2:     /// value right
      v[i]= gmn / exp_y0h;
      break;
    case 3:     /// error right
    case 1:     /// error left
      v[i] = sqrt(QUAD(gmn) * QUAD(y0err[i])/4. +
		  QUAD(gerr)) * exp_y0h;
      break;
    }
  }
}

/// constructor for T0_TDC (TDC module average, uses ROC file)
CheckinValues::CheckinValues(int lr, ReadRoc* rroc) {
  map<int,SlotAverage> a;
  int i0 = gCalib->GetSaveOffs();

  for (int i = 0; i < N_CHANNEL; i++){
    if(SC_Version_Flag == 1){
      if(i%57 > 47)
        continue;
      a[rroc->GetSlot(lr*288+i)] += (v[i] = gConst[0]->GetParameter(i, i0));
    }else{a[rroc->GetSlot(lr*N_CHANNEL+i)] += (v[i] = gConst[0]->GetParameter(i, i0));}
  }

  for (int i = 0; i < N_CHANNEL; i++) {
    if(SC_Version_Flag == 1){
      if(i%57 > 47)
        continue;
      v[i]-= a[rroc->GetSlot(lr*288+i)].getAverage();
    }else{v[i]-= a[rroc->GetSlot(lr*N_CHANNEL+i)].getAverage();}
  }
}


int CheckinValues::WriteMap(const char* filename) {
  ofstream fmap(filename);
  if (!fmap.good()) return -1;
  for (int i=0; i<N_CHANNEL; i++)
    fmap << v[i] << endl;
  return 0;
}
//------------------- CheckinValues ---------------------------

