// Autogenerated Class (Header File)
// Author : G.Gavalian
// Date   : Fri Feb 29 19:46:04 EST 2008
//

#ifndef __TEnergyLoss__
#define __TEnergyLoss__
#include <iostream>
#include <TROOT.h>
#include <TVector3.h>
#include <TLorentzVector.h>
#include <TObject.h>
#include "TFuncIntercept.h"
#include "TFuncLoss.h"

#define  S_PI  3.14159265359


namespace TELossTargetCell {
  static  double  LC[8] = {6.0, 4.5, 8.5, 8.5, 8.5  ,11.15, 19.15,  19.15};
  static  double  LS[8] = {6.0, 1.0, 5.0, 5.0, 6.85 , 8.0 , 16.0 ,  16.0};
  static  double  RC[8] = {3.0, 2.0, 2.0, 2.0, 1.4  , 0.0 ,  2.0 ,  2.0};
  static  double  RS[8] = {3.0, 4.0, 4.0, 4.0, 2.248, 4.0 ,  4.0 ,  4.0};
  static  double  WALL[8] = {0.032, 0.0277, 0.0277, 0.034, 0.0277, 0.0277, 0.0227,  0.0227};
};


class TEnergyLoss : public TObject {

private:

  double  fPAR_TargetOffset[3];
  double  fPAR_STOffset;

  int     fEloss_Cell;
  int     fEloss_iFlag;

  TFuncIntercept  fInter;
  TFuncLoss       fLoss;

  int             fDebugMode;


public:

TEnergyLoss ();
~TEnergyLoss ();

 TVector3  A2V(double pu[]);
 void      V2A(TVector3 v3,double pu[]);
 void   PrintWarning();
 void   SetDebugMode(int mode){fDebugMode = mode;};
 void   Init(int icell, int iflag);
 void   Init(const char *runname);
 int    DRound(double arg);
 void   LH2Targ(double vertex[],double cdir[],double *dist, double *dist1);
 void   CCylinder(double vertex[],double cdir[],double *dist);
 void   TargetCell(int icell, double vertex[],double cdir[],double *dist, double *dist1);
 void   AirGap(double vertex[],double cdir[],double xpoint[]);
 void   StartCounter(int icell,double vertex[],double cdir[],double *dist, double xpoint[]);
 void   ScatteringChamber(int icell, double vertex[], double cdir[],double *dist,int *iwin,double pos1[]);

 TVector3   GetVector(TVector3 v3_pout, double pmass, TVector3 v3_vrt, int iflag, int icell);

 TLorentzVector   GetVector(TLorentzVector _vl, TVector3 v3_vrt);

 void   SetOffsets(TVector3 tg,double st)
 {
   fPAR_TargetOffset[0] = tg.x();
   fPAR_TargetOffset[1] = tg.y();
   fPAR_TargetOffset[2] = tg.z();
   fPAR_STOffset     = st;
 };

 double sind(double angle){return sin(angle*S_PI/180.);}; 
 double cosd(double angle){return cos(angle*S_PI/180.);}; 
 double tand(double angle){return tan(angle*S_PI/180.);};

ClassDef(TEnergyLoss,1)


};

#endif
