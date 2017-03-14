// Photon class header file. -*- C++ -*-
// Author: Mike Williams 2/4/2004
#ifndef _Photon_H
#define _Photon_H
/** @file Photon.h
 * @brief Photon class definiton file.
 */
//_____________________________________________________________________________
// System Headers:
#include <iostream>
// ROOT Headers:
#include "TLorentzVector.h"
//_____________________________________________________________________________

class Photon : public TObject {

private:
  // Data Members (private):
  TLorentzVector _p4; ///< 4-Momentum
  Int_t _e_id;  ///< E paddle id number
  Int_t _t_id;  ///< T paddle id number
  Float_t _vertexTime; ///< Vertex time (propagated to the event vertex)
  Float_t _pull; ///< Pull quantity from kinematic fitting

  // Functions (private):
  // copies the Photon data members (not base class members)
  void _Copy(const Photon &__ph);

public:
  
  // Create/Copy/Destroy:

  Photon():TObject(){
    /// Default Constructor
  }

  Photon(const Photon &__ph):TObject(__ph){
    /// Copy Constructor
    this->_Copy(__ph);
  }

  virtual ~Photon(){
    /// Destructor
  }

  // assignment operator
  Photon& operator=(const Photon &__ph);
  
  // Functions:

  // Setters:
  
  inline void SetP4(const TLorentzVector &__p4) {
    /// Set the 4-momentum     
    _p4 = __p4;
  }

  inline void SetP4(double __erg){
    /// Set the 4-momentum 
    _p4.SetPxPyPzE(0.,0.,__erg,__erg);
  }

  inline void SetVertexTime(float __t) {
    /// Set the vertex time     
    _vertexTime = __t;
  }

  inline void SetE_Id(int __eid) {
    /// Set the e-counter paddle id     
    _e_id = __eid;
  }

  inline void SetT_Id(int __tid) {
    /// Set the t-counter paddle id      
    _t_id = __tid;
  }

  inline void SetPull(float __pull) {
    /// Set the pull quantity (from kinematic fitting)    
    _pull = __pull;
  }
  
  // Getters:

  inline const TLorentzVector& P4() const {
    /// Returns the 4-momentum     
    return _p4;
  }

  inline Float_t E() const {
    /// Returns the energy 
    return _p4.E();
  }

  inline Float_t VertexTime() const {
    /// Returns the vertex time      
    return _vertexTime;
  }

  inline Int_t E_Id() const {
    /// Returns the e-counter paddle id  
    return _e_id;
  }

  inline Int_t T_Id() const {
    /// Returns the t-counter paddle id     
    return _t_id;
  }

  inline Float_t Pull() const {
    /// Returns the pull quantity (from kinematic fitting)     
    return _pull;
  }

  // Functions:

  // sets all data members to 0.
  void Zero();

  // prints info to os
  void Print(std::ostream &__os = std::cout) const;

  //  ClassDef(Photon,1); // ROOT class definition macro
};
//_____________________________________________________________________________

// define an overload of << for Photon
std::ostream& operator<<(std::ostream &__os,const Photon &__ph);

//_____________________________________________________________________________

#endif /* _Photon_H */

