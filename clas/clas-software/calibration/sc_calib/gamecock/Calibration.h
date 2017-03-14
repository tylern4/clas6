#ifndef __CALIBRATION_H
#define __CALIBRATION_H

#include "ROOT.h"
#include "jdefaults.h"
#include <string>

using namespace std;

class Calibration {
  calibration_t caltype;     /// from the enum list
  int           nparFit;     /// numbers of parameter in fit
  int           nparSave;    /// numbers of parameter to save for base
  int           saveOffs;    /// first parameter to save
  bool          relative;    /// relative, needs add info at checkin
  item_t        item;
  string*       histfile;    /// histogram file
  double        defltpar[6]; /// start parameter
  double        lowlimit[6]; /// lower fit/slider limit
  double        upplimit[6]; /// upper fit/slider limit
  string*       name;
public:
  Calibration ();
  Calibration (calibration_t caltype_, item_t item_=i_unknown);
  calibration_t GetType ()   { return caltype; }
  int GetNparFit ()          { return nparFit; }
  int GetNparSave ()         { return nparSave; }
  int GetSaveOffs ()         { return saveOffs; }
  int GetNparLoad () { return ( caltype==c_gmean? nparSave+1 : nparSave);}
  bool   IsTwoDim ();
  bool   IsGraph ();
  bool   IsLogy ();
  bool   IsRootHist ();  /// false: assume input histograms are HBOOK format
  bool   IsRelative ()       { return relative; }
  const char*  GetName();
  const char*  GetSubsystem(int ipar);
  const char*  GetUncertainty(int ipar);
  const char*  GetItem(int lr=0);
  const char*  GetHistName(int index);
  const char*  GetFileName();
  const char*  GetWindowName();
  const char*  GetLatexFunction();
  double GetDefaultPar(int ipar);
  double GetUpperLimit(int ipar);
  double GetLowerLimit(int ipar);
  void   SetDefaultPar(int ipar, double value) { defltpar[ipar] = value; }
  void   SetUpperLimit(int ipar, double value) { upplimit[ipar] = value; }
  void   SetLowerLimit(int ipar, double value) { lowlimit[ipar] = value; }
  void   SetDefaultPar(TF1* fitfun);
  void   SetFitLimits(TF1* fitfun);
  void   SetHistFilename(string s) { histfile = new string(s); }
  int Parameter4Display(int iwindow);
  int MaxTableValues();
  int GetTableIndex(int index);
  int IsRight();
};

ostream& operator<< (ostream& os, Calibration& cal);
#endif
