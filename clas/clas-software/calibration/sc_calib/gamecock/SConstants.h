#ifndef __SCONSTANTS_H
#define __SCONSTANTS_H

#include "jdefine.h"
#include <fstream>
#include <TF1.h>
#include "Calibration.h"


class SConstants {
  int    nparset;
  double par[N_CHANNEL] [6];
  double err[N_CHANNEL] [6];
  double chisquare[N_CHANNEL];
  double upperLimit [6];
  double lowerLimit [6];
  char comment[80];
  bool OutOfRange(const int index);
public:
  SConstants();
  bool    IsParset() {return nparset > 0;}
  int     GetNparset() {return nparset;}
  double* GetParameters(int index);
  double* GetParameters(int sector, int stripe);
  double* GetLowerLimits() {return lowerLimit; }
  double* GetUpperLimits() {return upperLimit; }
  double  GetParameter(int index, int ipar) { return par[index] [ipar]; }
  double  GetParError (int index, int ipar) { return err[index] [ipar]; }
  double  GetChisquare(int index);
  double* GetParErrors(int index);
  void    SetParameter(const int index, const int ipar, const double value);
  void    SetParameters(const int index, const double par0, const double par1,
			const double par2=0, const double par3=0, 
			const double par4=0, const double par5=0);
  void    SetParErrors(const int index, const double err0, const double err1,
		       const double err2=0, const double err3=0, 
		       const double err4=0, const double err5=0);
  int     SetParameters(const int index, const TF1* f);
  void    SetParameter(char* filename, int column=1);
  void    SetUpperLimit(int ipar, double value) {upperLimit[ipar]=value;}
  void    SetLowerLimit(int ipar, double value) {lowerLimit[ipar]=value;}
  void    SetComment(char* com) {strcpy (comment, com);} 
  void    Mipadc2Gmean(int index);
};

#endif
