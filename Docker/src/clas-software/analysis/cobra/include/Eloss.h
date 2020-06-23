// Eloss C++ header file. -*- C++ -*-
#ifndef _Eloss_CXX_H
#define _Eloss_CXX_H
/** @file Eloss.h
 * @brief C++ wrappers for functions in the @a eloss package
 */
//_____________________________________________________________________________

// ROOT Headers:
#include "TVector3.h"
#include "TLorentzVector.h"

extern "C" {
#include <kinematics.h>
// Standard eloss header in CLAS_PACK/include
#include "eloss.h"
// Function wrapper in the eloss package (CLAS_PACK/eloss) 
void targcell_(int &icell,float &vert,float &cdir,float &dist,float &dist1);
}

#include "ClasRuns.h"
//_____________________________________________________________________________

/// Calculates the track length of target material the particle passed thru.
inline double XTargMat(const TVector3 __vertex,const TLorentzVector &__p4,
		       int __cell){

  float p_unit[3],vert[3],x = 0.,x1 = 0.;
  TVector3 p3unit = (__p4.Vect()).Unit();

  // unit vector in p-direction
  p_unit[0] = p3unit.Px();
  p_unit[1] = p3unit.Py();
  p_unit[2] = p3unit.Pz();

  // vertex position
  vert[0] = __vertex.X();
  vert[1] = __vertex.Y();
  vert[2] = __vertex.Z();

  targcell_(__cell,*vert,*p_unit,x,x1);

  return x;
}
//_____________________________________________________________________________

/// C++ wrapper for @a eloss momcor function.
/** Note: If the target offset of this call is different than the last call, 
 *  then InitEloss is called. InitEloss is called on the first call to this
 *  function.
 */
inline TLorentzVector Cpp_Momcor(const TLorentzVector &__p4meas,
				 const TVector3 &__vert,int __targMat,
				 int __targCell,int __runNumber){
  // check if InitEloss needs to be called
  static float targOffset = 666.;
  if(ClasRuns::Instance()->GetRunPeriod(__runNumber).TargetOffset() != targOffset){
    InitEloss(__runNumber);
  }
  vector4_t p4meas,p4cor;
  vector3_t vert;
  double mass = __p4meas.M();
  p4meas.space.x = __p4meas.Px();
  p4meas.space.y = __p4meas.Py();
  p4meas.space.z = __p4meas.Pz();
  p4meas.t = __p4meas.E();

  vert.x = __vert.X();
  vert.y = __vert.Y();
  vert.z = __vert.Z();

  p4cor = c_momcor(p4meas,mass,vert,__targMat,__targCell);
  TLorentzVector p4(p4cor.space.x,p4cor.space.y,p4cor.space.z,p4cor.t);
  return p4;
}
//_____________________________________________________________________________

#endif /* _Eloss_CXX_H */
