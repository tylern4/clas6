// Particle class header file. -*- C++ -*-
// Author: Mike Williams 2/4/2004
#ifndef _Particle_H
#define _Particle_H
/** @file Particle.h
 * @brief Particle class definition file.
 */
//_____________________________________________________________________________
// System Headers:
#include <iostream>
// ROOT Headers:
#include "TLorentzVector.h"
//_____________________________________________________________________________

// Global Types:
typedef std::string String;

//_____________________________________________________________________________

class Particle :public TObject {

private:
  // Data Members (private):
  TLorentzVector _p4; ///< 4-Momentum
  TVector3 _vertex; ///< Vertex position
  Int_t _sector; ///< Sector number particle traveled through in CLAS
  Int_t _sc_id; ///< 100*TOF scintillator id the particle hit + sc status
  Float_t _vertexTime; ///< Start Counter vertex time
  Float_t _tof; ///< Time of flight (sc time - vertex time of photon)
  Float_t _pathLen; ///< Path length of the track
  Float_t _pulls[3]; ///< Pull quantities from kinematic fitting
  Float_t _calcMass; ///< Calculated mass (from particle id)

  // Functions (private):
  // copies Particle data members
  void _Copy(const Particle &__p);

public:
  // Create/Copy/Destroy:

  Particle():TObject(){
    /// Default Constructor (calls Particle::Zero())
    this->Zero();
  }

  Particle(const Particle &__p):TObject(__p){
    /// Copy Constructor
    this->_Copy(__p);
  }

  virtual ~Particle(){
    /// Destructor
  }

  // assignment operator
  Particle& operator=(const Particle &__p);

  // Comparison Operators
  
  inline bool operator==(const Particle &__p) const {
    /// Comparison operator.
    for(int i = 0; i < 3; i++) if(_pulls[i] != __p._pulls[i]) return false;
    return (_p4 == __p._p4 && _vertexTime == __p._vertexTime
	    && _sector == __p._sector && _sc_id == __p._sc_id
	    && _tof == __p._tof && _pathLen == __p._pathLen
	    && _vertex == __p._vertex && _calcMass == __p._calcMass);
  }

  inline bool operator!=(const Particle &__p) const {
    /// Comparison operator
    return !(*this == __p);
  }

  // Functions:

  // Setters:

  inline void SetVertexTime(float __time) {
    /// Set the start counter vertex time     
    _vertexTime = __time;
  }

  inline void SetSector(int __sec) {
    /// Set the sector number     
    _sector = __sec;
  }

  inline void SetSC_Id(int __scId,int __scStatus = 0) {
    /// Set the time-of-flight scintilator paddle id     
    _sc_id = 100*__scId + __scStatus;
  }

  inline void SetP4(const TLorentzVector &__p4) {
    /// Set the 4-momentum      
    _p4 = __p4;
  }

  inline void SetVertex(const TVector3 &__vert) {
    /// Set the vertex position      
    _vertex = __vert;
  }

  inline void SetTOF(float __tof) {
    /// Set the time-of-flight      
    _tof = __tof;
  }

  inline void SetPathLength(float __pathlen) {
    /// Set the path length      
    _pathLen = __pathlen;
  }

  inline void SetPulls(const float __pulls[3]){
    /// Set the pull distributions (from kinematic fitting)      
    for(int i = 0; i < 3; i++) _pulls[i] = __pulls[i];
  }

  inline void SetCalcMass(float __mass) {
    /// Set the calculated mass (from particle id)     
    _calcMass = __mass;
  }

  inline void SetMass(float __mass){
    /// Set the mass in @a p4 (ie. set \f$ E = \sqrt(p^2 + m^2)\f$      
    _p4.SetE(sqrt(_p4.P()*_p4.P() + __mass*__mass));
  }

  // Getters:

  inline const TLorentzVector& P4() const {
    /// Returns the 4-momentum      
    return _p4;
  }

  inline const TVector3& Vertex() const {
    /// Returns the vertex position      
    return _vertex;
  }

  inline Double_t Px() const {
    /// Returns the x-component 4-momentum      
    return _p4.Px();
  }
  
  inline Double_t Py() const {
    /// Returns the y-component 4-momentum      
    return _p4.Py();
  }

  inline Double_t Pz() const {
    /// Returns the z-component 4-momentum      
    return _p4.Pz();
  }

  inline Double_t E() const {
    /// Returns the energy-component 4-momentum      
    return _p4.E();
  }

  inline Int_t Sector() const {
    /// Returns the sector number      
    return _sector;
  }

  inline Int_t SC_Id() const {
    /// Returns the time-of-flight scintilator paddle id     
    return _sc_id/100;
  } 

  inline Int_t SC_Status() const {
    /// Returns the scintilator paddle status flag (33 is @a good)
    return (_sc_id - this->SC_Id()*100);
  }

  inline Float_t VertexTime() const {
    /// Returns the start counter vertex time      
    return _vertexTime;
  }

  inline Float_t TOF() const {
    /// Returns the time-of-flight     
    return _tof;
  }

  inline Float_t PathLength() const {
    /// Returns the path length     
    return _pathLen;
  }

  inline Float_t Pull(int __n) const {
    /// Returns the pull distributions (from kinematic fitting) 
    /** @a n = 0 \f$ \rightarrow |p| \f$     
     *  @a n = 1 \f$ \rightarrow \lambda \f$     	
     *  @a n = 2 \f$ \rightarrow \phi \f$     
     */
    return _pulls[__n];
  }

  inline Float_t CalcMass() const {
    /// Returns the calculated mass 
    return _calcMass;
  }

  // Functions:

  // zero all data members
  void Zero();

  // print info to os
  void Print(std::ostream &__os = std::cout) const;

  //  ClassDef(Particle,1); // ROOT class definition macro
};
//_____________________________________________________________________________

// overload of << for Particle
std::ostream& operator<<(std::ostream &__os,const Particle &__part);

//_____________________________________________________________________________

#endif /* _Particle_H */


