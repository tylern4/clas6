// Author: Mike Williams (2/4/2004)
#include "Particle.h"
//_____________________________________________________________________________

/** @file Particle.C
 * @brief Particle class source file.
 */
//_____________________________________________________________________________

/** \class Particle
 * \brief Utility class used by Event to store charged particle info.
 *
 * This class is a utiltiy class used by Event to store charged particle
 * information. It stores the particle's 4-momentum, start counter vertex time,
 * TOF scintillator id, sector number, time of flight, track length, 
 * id (as a GEANT id number) and pull distributions from kinematic fitting.
 *
 * The 4-momentum is stored as a ROOT TLorentzVector object. Thus to obtain
 * any kinematic quantity, just get a reference to the 4-momentum using
 * Particle::P4(), then the appropriate ROOT TLorentzVector functions.
 *
 * <b> Examples </b> 
 *
 * <!--
 * Particle part; 
 * ... assume that it's 4-momentum is set some how ...
 * double phi = part.Phi(); // azimuthal angle
 * double gamma = part.Gamma(); // 1/sqrt(1 - beta^2)
 * ... to boost to a different frame ...
 * TVector3 bv(x,y,z); // boost vector, set to whatever 
 * TLorentzVector p4 = part.P4();
 * p4.Boost(bv);  // now p4 is in frame of lab boosted using bv
 * double phi_bvFrame = p4.Phi(); // phi in this frame
 *
 * -->
 * \include Particle.ex
 *
 */
//_____________________________________________________________________________

//ClassImp(Particle); // ROOT class implementation macro

//_____________________________________________________________________________

void Particle::_Copy(const Particle &__p){
  /// Copy the data members of @a __p (just the @a Particle members).
  _p4 = __p._p4;
  _vertexTime = __p._vertexTime;
  _sector = __p._sector;
  _sc_id = __p._sc_id;
  _tof = __p._tof;
  _pathLen = __p._pathLen;
  _vertex = __p._vertex;
  _calcMass = __p._calcMass;
  for(int i = 0; i < 3; i++) _pulls[i] = __p._pulls[i];
}
//_____________________________________________________________________________

Particle& Particle::operator=(const Particle& __p){
  /// Assignment Operator.
  this->TObject::operator=(__p);
  this->_Copy(__p);

  return *this;
}
//_____________________________________________________________________________

void Particle::Zero(){
  /// Set ALL Particle data members to zero.
  _p4.SetPxPyPzE(0.,0.,0.,0.);
  _vertex.SetXYZ(0.,0.,0.);
  _vertexTime = 0.0;
  _sector = 0;
  _sc_id = 0;
  _tof = 0.;
  _pathLen = 0.;
  _calcMass = 0.;
  for(int i = 0; i < 3; i++) _pulls[i] = 0.;
}
//_____________________________________________________________________________

void Particle::Print(std::ostream &__os) const {
  /// Print contents of this Particle object to @a os.
  /** 
   * The std::ostream object, @a os, defaults to std::cout. Thus, 
   * Particle::Print() prints the info to the screen.
   *
   */
  __os << "[P4: (" << this->Px() << ","
       << this->Py() << "," << this->Pz() << "," << this->E() << ")" 
       << " Sector: " << this->Sector() <<  " ST vtime: " 
       << this->VertexTime() <<  " SC: ID: " << this->SC_Id() << "[" 
       << this->SC_Status() << "]" 
       << " TOF: " << this->TOF() << " Path Length: " << this->PathLength()
       << "]" << std::endl;
}
//_____________________________________________________________________________

/// Overload of std::ostream::operator<< for Particle.
std::ostream& operator<<(std::ostream &__os,const Particle &__p){
  __p.Print(__os);
  return __os;
}
//_____________________________________________________________________________
