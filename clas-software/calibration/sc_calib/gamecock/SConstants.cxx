#include <cmath>
#include "Calibration.h"
#include "SConstants.h"

extern Calibration* gCalib;

using namespace std;

SConstants::SConstants() : nparset(0) {
  memset (par, 0, sizeof(par));
  memset (err, 0, sizeof(err));
  memset (chisquare, 0, sizeof(chisquare));
  memset (upperLimit, 0, sizeof(upperLimit));
  memset (lowerLimit, 0, sizeof(lowerLimit));
  memset (comment, 0, sizeof(comment));
}

bool SConstants::OutOfRange(const int index) { 
  if (index < 0)    return true;
  if (index >= N_CHANNEL) return true;
  return false;
};

double SConstants::GetChisquare(const int index) { 
  if (OutOfRange(index)) throw "SConstants::GetChisquare: index out of range";
  return chisquare[index];
}

double* SConstants::GetParErrors(const int index) { 
  if (OutOfRange(index)) throw "SConstants::GetParErrors: index out of range";
  return err[index];
}

double* SConstants::GetParameters(const int index) { 
  if (OutOfRange(index)) throw "SConstants::GetParameters: index out of range";
  return par[index];
}

double* SConstants::GetParameters(int sector, int stripe) { 
  return GetParameters( (sector-1) * N_SECTOR + (stripe-1));
}

/// set a row from function parameter
int SConstants::SetParameters(const int index, const TF1* ff) {
  nparset++;
  for (int i=0; i<gCalib->GetNparFit(); i++) { 
    double vpar = ff->GetParameter(i);
    double verr = ff->GetParError(i);
    if (isnan(vpar) || isnan(verr) || isinf(vpar) || isinf(verr)) 
      return -1;
    par[index] [i] = vpar;
    err[index] [i] = verr;
  }
  chisquare[index] = ff->GetChisquare();
  return 0;
}

/// set a single value
void SConstants::SetParameter(const int index, const int ipar, const double value) {
  nparset++;
  if (OutOfRange(index)) 
    throw "SConstants::SetParameter: index out of range";
  if (ipar >= 6 || ipar < 0) 
    throw "SConstants::SetParameter: ipar out of range";
  par [index] [ipar] = value;
}

/// set a whole row
void SConstants::SetParameters(const int index, const double par0, const double par1,
			      const double par2, const double par3, 
			      const double par4, const double par5) {
  nparset++;
  par[index] [0] = par0;
  par[index] [1] = par1;
  par[index] [2] = par2;
  par[index] [3] = par3;
  par[index] [4] = par4;
  par[index] [5] = par5;
}

void SConstants::SetParErrors(const int index, const double err0, const double err1,
			      const double err2, const double err3, 
			      const double err4, const double err5) {
  err[index] [0] = err0;
  err[index] [1] = err1;
  err[index] [2] = err2;
  err[index] [3] = err3;
  err[index] [4] = err4;
  err[index] [5] = err5;  
}


/// load a column from file
void SConstants::SetParameter(char* filename, int column) {
  ifstream ff(filename);
  for (int i=0; i<N_CHANNEL; i++) {
    if (! ff.good()) { 
      cerr << "can't read " << filename << endl; 
      throw "SConstants::SetParameter: can't open file"; 
    }
    ff >> par[i] [column];
    nparset++;
  }
}

void SConstants::Mipadc2Gmean(int index) {
  double fpar = sqrt(par[index][1] * par[index][2]);
  SetParameters(index, 0., fpar, 0., 0., 0.);
}
